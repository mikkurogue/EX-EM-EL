//! XML tokenizer and parser

const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ascii = std.ascii;
const eql = std.mem.eql;

/// Recognized tokens that we use for the XML
const TokenType = enum {
    LeftAngleBracket,
    RightAngleBracket,
    Slash,
    Equal,
    String,
    Bang,
    Text,
    Unknown,
    AttrName,
    AttrValue,
    EOF,
};

/// The token itself.
/// lexeme is the value of the token, or null. If it is null, then it is a symbol token
/// and not of importance for the result when processing the data.
/// token_type is the corresponding token type for the lexeme
const Token = struct {
    lexeme: ?[]const u8 = null,
    token_type: TokenType,
};

/// Scanner struct to scan the input. This contains the following members
/// `current` is the current token we are iterating over, default is 0.
/// `start` is the start of the tokens, default is 0.
/// `input` is the input string of XML to parse.
const Scanner = struct {
    current: usize = 0,
    start: usize = 0,
    input: []const u8,

    /// Advance to the next token - returns the character
    fn advance(self: *Scanner) u8 {
        const result = self.input[self.current];
        self.current += 1;
        return result;
    }

    /// peek the next token, but seeing what the next token will be and return the character
    fn peek(self: *Scanner) u8 {
        return self.input[self.current];
    }

    /// check if we are at the end of the input
    /// returns a boolean
    fn end(self: *Scanner) bool {
        return self.current >= self.input.len;
    }

    /// Simplified tokenizer to map the symbols to the correct tokens
    /// Note we dont handle every case here as of yet.
    /// The main cases for < > / = ! " are handled, and EOF is implcitly handled once the loop
    /// completes.
    ///
    /// Other tokens are to be added if they deem to be necessary.
    pub fn scan_tokens(self: *Scanner, allocator: Allocator) Allocator.Error!ArrayList(Token) {
        var output = ArrayList(Token).init(allocator);

        while (!self.end()) {
            const char: u8 = self.advance();

            const token: Token = switch (char) {
                '<' => Token{ .token_type = TokenType.LeftAngleBracket },
                '>' => Token{ .token_type = TokenType.RightAngleBracket },
                '/' => Token{ .token_type = TokenType.Slash },
                '=' => Token{ .token_type = TokenType.Equal },
                '!' => Token{ .token_type = TokenType.Bang },

                '"' => blk: {
                    var c: u8 = self.advance();
                    while (c != '"') {
                        c = self.advance();
                    }

                    break :blk Token{ .lexeme = self.input[self.start + 1 .. self.current - 1], .token_type = TokenType.AttrValue };
                },
                else => blk: {
                    while (ascii.isAlphanumeric(self.peek()) or self.peek() == '-') {
                        _ = self.advance();
                    }

                    if (self.peek() == '=') {
                        break :blk Token{ .lexeme = self.input[self.start..self.current], .token_type = TokenType.AttrName };
                    }

                    break :blk Token{ .lexeme = self.input[self.start..self.current], .token_type = TokenType.Text };
                },
            };

            try output.append(token);
            self.start = self.current;
        }

        try output.append(.{ .token_type = TokenType.EOF, .lexeme = "<EOF>" });
        return output;
    }
};

/// Potential errors the Parser can throw
const ParserError = error{ UnexpectedEOF, UnexpectedClosingTag, UnexpectedToken, OutOfMemory };

/// The diagnostics for the parser for feedback during iteration
const ParseDiagnostics = struct {
    line: usize,
    token: ?Token,
};

/// Parser struct that parses the tokens and returns a correct HtmlTag struct.
/// Members are:
/// `allocator` the allocator required for memory allocation methods
/// `tokens` the list of tokens that are available to iterate over to extract values
/// `current` the current token we are looking at
/// `current_tag` the current token tag, is nullable and defaults to null. If this is null, then we arent looking at a tag name.
/// A tag name is for instance the word `div` in this example: <div>
/// `line` the current line number we are on
const Parser = struct {
    allocator: Allocator,
    tokens: ArrayList(Token),
    current: usize = 0,
    current_tag: ?[]const u8 = null,
    line: usize = 0,

    /// check if the parser has reached the end of the token list
    fn end(self: *Parser) bool {
        return self.current >= self.tokens.items.len;
    }

    /// peek the next token, if we are at the end return the second to last token as EOF
    fn peek(self: *Parser) Token {
        if (self.end()) return self.tokens.items[self.current - 1];
        return self.tokens.items[self.current];
    }

    /// Advance to the next token in the list. If we are at the end then return second to last token as EOF
    fn advance(self: *Parser) Token {
        if (self.end()) return self.tokens.items[self.current - 1];

        const tmp = self.tokens.items[self.current];
        self.current += 1;
        return tmp;
    }

    /// check if the token type is equal to what we are expecting
    fn check(self: *Parser, token_type: TokenType) bool {
        if (self.end()) return false;

        return self.peek().token_type == token_type;
    }

    /// match the token type to what we are expecting and return the next token. If it is mismatched, returns ParserError.UnexpectedToken
    fn match(self: *Parser, token_type: TokenType) ParserError!Token {
        if (self.check(token_type)) {
            return self.advance();
        }
        return ParserError.UnexpectedToken;
    }

    /// Parse the current tag and its values.
    /// eg: <div class="p-0">hello</div>
    /// is parsed per token.
    /// Then attribute and return the correct HtmlTag struct from this.
    fn parse_tag(self: *Parser) ParserError!HtmlTag {
        _ = try self.match(TokenType.LeftAngleBracket);

        const tag_name = (try self.match(TokenType.Text)).lexeme orelse "";

        // TODO: Fix this to be of type ?[]const HtmlAttr
        // for now just support singular attributes
        const attrs: ?HtmlAttr = try self.parse_attrs();

        _ = try self.match(TokenType.RightAngleBracket);

        // FIXME: Need to fix this to support child elements
        // if (self.check(TokenType.LeftAngleBracket)) {
        //     std.log.warn("{any}", .{self.check(TokenType.LeftAngleBracket)});
        //     std.log.info("CHILD ELEMENT FOUND", .{});
        //
        //     _ = try self.parse_child_tag();
        // }

        // Parse and get the value here inside the tag
        const value: []const u8 = try self.parse_value();

        _ = try self.match(TokenType.LeftAngleBracket);
        _ = try self.match(TokenType.Slash);

        const close_tag_name = (try self.match(TokenType.Text)).lexeme orelse "";

        _ = try self.match(TokenType.RightAngleBracket);

        if (!eql(u8, tag_name, close_tag_name)) {
            std.log.err("open and close tags do not match: {s} and {s}", .{ tag_name, close_tag_name });
            return ParserError.UnexpectedClosingTag;
        }

        return HtmlTag{ .name = tag_name, .attrs = attrs, .value = value };
    }

    /// Parse a child tag within a tag.
    /// FIXME: This causes a memory leak somewhere
    fn parse_child_tag(self: *Parser) ParserError!HtmlTag {
        _ = try self.match(TokenType.LeftAngleBracket);
        const child_tag_name = (try self.match(TokenType.Text)).lexeme orelse "";

        const attrs: ?HtmlAttr = try self.parse_attrs();

        _ = try self.match(TokenType.RightAngleBracket);

        const child_val: []const u8 = try self.parse_value();

        _ = try self.match(TokenType.LeftAngleBracket);
        _ = try self.match(TokenType.Slash);

        const child_closing_tag_name = (try self.match(TokenType.Text)).lexeme orelse "";
        _ = try self.match(TokenType.RightAngleBracket);

        if (!eql(u8, child_tag_name, child_closing_tag_name)) {
            std.log.err("child open and close tags do not match: {s} and {s}", .{ child_tag_name, child_closing_tag_name });
        }

        return HtmlTag{
            .name = child_tag_name,
            .attrs = attrs,
            .value = child_val,
        };
    }

    // TODO: make this return a !?[]const HtmlAttr
    /// Parse the attributes for a tag, eg: <div class="p-0">
    /// would be parsed to return the class as the attribute name, and the p-0 as the attribute value
    /// This operation trims the whitespaces from the attribute names as we do not have a token type for
    /// TokenType.Whitespace, as we need to support string values that can be multiple words delimited by whitespace.
    fn parse_attrs(self: *Parser) !?HtmlAttr {
        if (self.peek().token_type != TokenType.AttrName) {
            return null;
        }

        const attr_name = (try self.match(TokenType.AttrName)).lexeme orelse "";
        if (attr_name.len == 0) {
            return null;
        }

        // this is the check for equal sign
        _ = try self.match(TokenType.Equal);

        const attr_val = (try self.match(TokenType.AttrValue)).lexeme orelse "";
        if (attr_val.len == 0) {
            return null;
        }

        // return the attribute, trim the spaces from the name as its not allowed anyway
        return HtmlAttr{ .name = std.mem.trim(u8, attr_name, " "), .value = attr_val };
    }

    // TODO: parse multi-text (delimiter is a " ")
    /// Parse the value of the inside of a tag, eg: <div>hello</div> would return the string hello.
    /// Currently this only supports values of up to 2 strings, so <div>hello world</div> is supported but anything more is not (yet)
    /// This result requires to be freed on implmentation. See the test "parse tag with value inside" for an implementation example.
    fn parse_value(self: *Parser) ParserError![]const u8 {
        var tag = ArrayList(u8).init(self.allocator);
        defer tag.deinit();

        // if the token isnt a text then there is no value
        if (self.peek().token_type != TokenType.Text) {
            return "";
        }

        const tag_value = (try self.match(TokenType.Text)).lexeme orelse "";
        if (tag_value.len == 0) {
            return "";
        }

        try tag.appendSlice(tag_value);

        // check if there is another tag value
        if (self.peek().token_type == TokenType.Text) {
            const secondary_tag_value = (try self.match(TokenType.Text)).lexeme orelse "";

            try tag.appendSlice(secondary_tag_value);
        }

        return tag.toOwnedSlice();
    }

    /// Parse the entire input given to the Parser struct and return its output.
    pub fn parse(self: *Parser, _: *ParseDiagnostics) (Allocator.Error || ParserError)!ArrayList(HtmlTag) {
        self.line = 0;
        var output = ArrayList(HtmlTag).init(self.allocator);

        while (!self.end()) {
            const token = self.peek();

            if (token.token_type == TokenType.EOF) {
                break;
            }

            const tag = try self.parse_tag();
            try output.append(tag);
        }

        return output;
    }
};

// TODO: make attrs prop be of type ?[]const HtmlAttr
// for now we only support single attribute
const HtmlTag = struct {
    name: []const u8,
    attrs: ?HtmlAttr,
    value: []const u8,
};

const HtmlAttr = struct {
    name: []const u8,
    value: []const u8,
};

/// Test method to map the tokens to the lexemes returned, remember that lexemes are nullable so if its an empty string
/// it means the lexeme has no value to be mapped to a token so it will stay empty
fn printTokens(tokens: *const std.ArrayList(Token)) !void {
    for (tokens.items) |i| {
        std.debug.print("{s}: '{s}'\n", .{ switch (i.token_type) {
            TokenType.LeftAngleBracket => "LeftAngleBracket",
            TokenType.RightAngleBracket => "RightAngleBracket",
            TokenType.Slash => "Slash",
            TokenType.Equal => "Equal",
            TokenType.String => "String",
            TokenType.Unknown => "Unknown",
            TokenType.Bang => "Bang",
            TokenType.Text => "Text",
            TokenType.EOF => "<EOF>",
            TokenType.AttrValue => "Attribute Value",
            TokenType.AttrName => "Attribute name",
        }, i.lexeme orelse "" });
    }
}

// Tests

/// Test method to test parsing the input. Is essentially a wrapper function for simplicity, this should not be used necesarily
/// but is a good reference for future implementation
fn test_parse(input: []const u8, allocator: Allocator) !ArrayList(HtmlTag) {
    var scanner = Scanner{ .input = input };
    var tokens = try scanner.scan_tokens(allocator);

    defer tokens.deinit();

    var parser = Parser{ .tokens = tokens, .allocator = allocator };
    var parse_diag: ParseDiagnostics = ParseDiagnostics{ .line = 0, .token = Token{ .lexeme = null, .token_type = TokenType.EOF } };

    const tags = try parser.parse(&parse_diag);
    return tags;
}

test "parse open tag without attributes" {
    const tags = try test_parse("<hello></hello>", std.testing.allocator);
    defer tags.deinit();
    const root = tags.items[0];

    try std.testing.expect(std.mem.eql(u8, root.name, "hello"));
}

test "parse open tag with attributes" {
    const tags = try test_parse("<hello id=\"test_id\"></hello>", std.testing.allocator);
    defer tags.deinit();
    const root = tags.items[0];

    try std.testing.expect(std.mem.eql(u8, root.attrs.?.name, "id"));
    try std.testing.expect(std.mem.eql(u8, root.attrs.?.value, "test_id"));
}

test "parse tag with value inside" {
    const input = "<hello>something</hello>";

    const tags = try test_parse(input, std.testing.allocator);
    defer tags.deinit();
    const root = tags.items[0];

    defer std.testing.allocator.free(root.value);

    try std.testing.expect(std.mem.eql(u8, root.value, "something"));
}

test "parse tag with string of words inside" {
    const input = "<hello>something here</hello>";

    const tags = try test_parse(input, std.testing.allocator);
    defer tags.deinit();
    const root = tags.items[0];

    defer std.testing.allocator.free(root.value);

    try std.testing.expect(std.mem.eql(u8, root.value, "something here"));
}

// FIXME: Need to fix this test, test implementation is fine its the core logic
// test "parse nested tag" {
//     const input = "<parent><child>hello world</child></parent>";
//
//     const tags = try test_parse(input, std.testing.allocator);
//     defer tags.deinit();
//     const root = tags.items[0];
//
//     defer std.testing.allocator.free(root.value);
//
//     try std.testing.expect(std.mem.eql(u8, root.value, "something"));
// }

/// Open an XML file.
/// This is responsible then for scanning, tokenizing and parsing the file.
/// the result of this, should then be curated and placed into the file stream from
/// fn create_temp_storage_file()
pub fn open_xml(path: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    return "opened xml file";
}

/// Create and return the file to stream into it.
/// The return result must have defer X.close() to complete lifecycle
pub fn create_temp_storage_file() !std.fs.File {
    const temp = try std.fs.cwd().makeDir("temp-store", .{});
    temp.close();

    const temp_file = try std.fs.cwd().createFile("temp/temp-store/temp-store.xml", .{});

    return temp_file;
}
