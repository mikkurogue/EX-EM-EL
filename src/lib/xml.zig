//! XML tokenizer and parser
//! Recommended usage for these are to use the ArenaAllocator to make
//! life simple.
//! Pass the ArenaAllocator as the main allocator to both Tokenizer and Parser.
//! This will make life much simpler to free memory, especially when working
//! with a deeply nested XML/HTML structure.

const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const ascii = std.ascii;
const eql = std.mem.eql;

// rewrite Scanner to use this instead.
pub const TokenType = enum {
    XMLIdentifier, // <? opening tag - we can skip this
    OpeningTag, // e.g., <tag>
    ClosingTag, // e.g., </tag>
    SelfClosingTag, // e.g., <tag/>
    Equal, // e.g., =
    Slash, // e.g., /
    Bang, // e.g., !
    TagName, // e.g., "tag" in <tag>
    Value, // Text between tags, e.g., "Hello" in <tag>Hello</tag>
    AttributeName, // e.g., "id" in <tag id="123">
    AttributeValue, // e.g., "123" in <tag id="123">
    Comment,
    Dash,
    NewLine,
    Whitespace,
    EOF, // End of file
};

/// Recognized tokens that we use for the XML
/// The token itself.
/// lexeme is the value of the token, or null. If it is null, then it is a symbol token
/// and not of importance for the result when processing the data.
/// token_type is the corresponding token type for the lexeme
pub const Token = struct {
    lexeme: ?[]const u8 = null,
    token_type: TokenType,
};

/// Scanner struct to scan the input. This contains the following members
/// `current` is the current token we are iterating over, default is 0.
/// `start` is the start of the tokens, default is 0.
/// `input` is the input string of XML to parse.
pub const Tokenizer = struct {
    current: usize = 0,
    start: usize = 0,
    input: []const u8,
    line: usize = 1,

    /// Advance to the next token - returns the character
    fn next(self: *Tokenizer) u8 {
        const result = self.input[self.current];
        self.current += 1;

        if (result == '\n') {
            self.line += 1;
        }

        return result;
    }

    /// peek the next token, but seeing what the next token will be and return the character
    fn peek(self: *Tokenizer) u8 {
        return self.input[self.current];
    }

    /// peek n places forward. ff stands for fast-forward
    fn peek_ff(self: *Tokenizer, ff: usize) u8 {
        if (self.current + ff < self.input.len) {
            return self.input[self.current + ff];
        }

        return 0; // sentinel
    }

    /// check if we are at the end of the input
    /// returns a boolean
    fn end(self: *Tokenizer) bool {
        return self.current >= self.input.len - 1;
    }

    fn skip_whitespace(self: *Tokenizer) void {
        while (!self.end() and std.ascii.isWhitespace(self.peek())) {
            _ = self.next();
        }
    }

    fn skip_xml_decl(self: *Tokenizer) void {
        if (self.peek() == '<' and self.peek_ff(1) == '?') {
            _ = self.next();
            _ = self.next();

            while (!self.end()) {
                if (self.peek() == '?' and self.peek_ff(1) == '>') {
                    _ = self.next();
                    _ = self.next();
                    break;
                }

                _ = self.next();
            }
        }
    }

    /// FIXME: implement this properly by skipping comments outright.
    fn skip_comment(self: *Tokenizer) void {
        _ = self;
    }

    pub fn tokenize(self: *Tokenizer, allocator: Allocator) anyerror!ArrayList(Token) {
        var output = ArrayList(Token).init(allocator);

        self.skip_xml_decl();

        while (!self.end()) {
            self.skip_whitespace();
            const char: u8 = self.next();

            const token: Token = switch (char) {
                '\n' => Token{ .token_type = .NewLine },
                '<' => openTag: {
                    // handle closing tags
                    if (self.peek() == '/') {
                        _ = self.next();
                        const start = self.current;
                        while (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '-') {
                            _ = self.next();
                        }
                        const tag_name = self.input[start..self.current];
                        _ = self.next();
                        break :openTag Token{
                            .token_type = .ClosingTag,
                            .lexeme = tag_name,
                        };
                    } else {
                        // handle opening tags
                        const start = self.current;
                        while (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '-') {
                            _ = self.next();
                        }
                        const tag_name = self.input[start..self.current];
                        try output.append(Token{ .token_type = .OpeningTag, .lexeme = tag_name });

                        // parse attributes
                        while (!self.end()) {
                            // skip whitespace
                            while (std.ascii.isWhitespace(self.peek())) _ = self.next();

                            if (self.peek() == '/') {
                                _ = self.next();
                                _ = self.next();
                                break :openTag Token{
                                    .token_type = .SelfClosingTag,
                                };
                            }

                            if (self.peek() == '>') {
                                _ = self.next();
                                break;
                            }

                            // attrs
                            const attr_start = self.current;
                            while (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '-') {
                                _ = self.next();
                            }
                            const attr_name = self.input[attr_start..self.current];

                            if (self.peek() == '=') {
                                _ = self.next();
                                if (self.peek() == '"') {
                                    _ = self.next(); // consume opening quote
                                    const val_start = self.current;
                                    while (self.peek() != '"') {
                                        _ = self.next();
                                    }
                                    const attr_val = self.input[val_start..self.current];
                                    _ = self.next(); // consume closing quote

                                    // dump the attr tokens
                                    try output.append(Token{
                                        .token_type = .AttributeName,
                                        .lexeme = attr_name,
                                    });
                                    try output.append(Token{
                                        .token_type = .AttributeValue,
                                        .lexeme = attr_val,
                                    });
                                }
                            }
                        }

                        continue;
                    }
                },
                '/' => slashTag: {
                    if (self.peek() == '>') {
                        // handle self-closing tag
                        _ = self.next();
                        break :slashTag Token{
                            .token_type = .SelfClosingTag,
                        };
                    }
                    break :slashTag Token{ .token_type = .Slash };
                },
                '=' => Token{ .token_type = .Equal },
                '!' => Token{ .token_type = .Bang },
                else => valueBlock: {
                    // handle text values
                    const start = self.current - 1;
                    while (!self.end() and self.peek() != '<') {
                        _ = self.next();
                    }
                    break :valueBlock Token{
                        .token_type = .Value,
                        .lexeme = self.input[start..self.current],
                    };
                },
            };

            try output.append(token);
            self.start = self.current;
        }

        // Append EOF only after finishing the tokenization process
        try output.append(.{ .token_type = .EOF, .lexeme = "<EOF>" });
        return output;
    }
};

/// Potential errors the Parser can throw
pub const ParserError = error{ MismatchedClosingTag, InvalidAttribute, UnexpectedEOF, UnexpectedClosingTag, UnexpectedToken, OutOfMemory, InvalidToken, NoTagName };

/// Parser struct that parses the tokens and returns a correct HtmlTag struct.
/// Members are:
/// `allocator` the allocator required for memory allocation methods
/// `tokens` the list of tokens that are available to iterate over to extract values
/// `current` the current token we are looking at
/// `current_tag` the current token tag, is nullable and defaults to null. If this is null, then we arent looking at a tag name.
/// A tag name is for instance the word `div` in this example: <div>
/// `line` the current line number we are on
pub const Parser = struct {
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
    fn next(self: *Parser) Token {
        if (self.end()) return self.tokens.items[self.current - 1];

        const tmp = self.tokens.items[self.current];
        self.current += 1;
        return tmp;
    }

    fn skip_newline(self: *Parser) void {
        while (!self.end() and self.peek().token_type == .NewLine) {
            _ = self.next();
        }
    }

    /// TODO: add line counter for when we encounter \n
    fn parse_tag(self: *Parser, level: usize) ParserError!HtmlTag {
        const opening_token = self.next();

        if (opening_token.token_type != .OpeningTag) {
            return ParserError.InvalidToken;
        }

        const tag_name = opening_token.lexeme orelse "";

        std.log.warn("opening tag: {s}", .{tag_name});

        var attrs = ArrayList(HtmlAttr).init(self.allocator);

        while (self.peek().token_type == .AttributeName) {
            const attr_name = self.next().lexeme orelse "";

            if (self.peek().token_type != .AttributeValue) {
                attrs.deinit();
                return ParserError.InvalidAttribute;
            }

            const attr_val = self.next().lexeme orelse "";
            try attrs.append(HtmlAttr{
                .name = attr_name,
                .value = attr_val,
            });
        }

        var children = ArrayList(HtmlTag).init(self.allocator);

        var value: []const u8 = "";
        while (!self.end() and self.peek().token_type != .ClosingTag) {
            const child_level = level + 1;
            if (self.peek().token_type == .OpeningTag) {
                try children.append(try self.parse_tag(child_level));
            } else if (self.peek().token_type == .Value) {
                value = self.next().lexeme orelse "";
            } else {
                attrs.deinit();
                children.deinit();
                return ParserError.InvalidToken;
            }
        }

        const closing_token = self.next();

        std.log.warn("Closing token tag {?s}", .{closing_token.lexeme.?});

        if (closing_token.token_type != .ClosingTag or
            !std.mem.eql(u8, tag_name, closing_token.lexeme orelse ""))
        {
            attrs.deinit();
            children.deinit();
            return ParserError.MismatchedClosingTag;
        }

        return HtmlTag{
            .level = level,
            .name = tag_name,
            .attrs = if (attrs.items.len > 0) try attrs.toOwnedSlice() else null,
            .value = value,
            .children = if (children.items.len > 0) try children.toOwnedSlice() else null,
        };
    }

    /// Parse the entire input given to the Parser struct and return its output.
    pub fn parse(
        self: *Parser,
    ) (Allocator.Error || ParserError)!ArrayList(HtmlTag) {
        var output = ArrayList(HtmlTag).init(self.allocator);

        while (!self.end()) {
            self.skip_newline();
            if (self.end()) break;
            const token = self.peek();

            if (token.token_type == .EOF) {
                break;
            }

            // start counting from 1.
            // level 1 is root level.
            // level 0 would be document level, and we dont care for that
            const tag = try self.parse_tag(1);
            try output.append(tag);
        }

        return output;
    }
};

const HtmlTag = struct { name: []const u8, level: usize, attrs: ?[]HtmlAttr, value: []const u8, children: ?[]HtmlTag };
const HtmlAttr = struct {
    name: []const u8,
    value: []const u8,
};

/// Test method to map the tokens to the lexemes returned, remember that lexemes are nullable so if its an empty string
/// it means the lexeme has no value to be mapped to a token so it will stay empty
pub fn printTokens(tokens: *const std.ArrayList(Token)) !void {
    for (tokens.items) |i| {
        std.debug.print("{s}: '{s}'\n", .{ switch (i.token_type) {
            .OpeningTag => "OpeningTag",
            .ClosingTag => "ClosingTag",
            .SelfClosingTag => "SelfClosingTag",
            .Value => "Value",
            .TagName => "TagName",
            .Slash => "Slash",
            .Equal => "Equal",
            .Bang => "Bang",
            .EOF => "<EOF>",
            .AttributeValue => "Attribute Value",
            .AttributeName => "Attribute name",
        }, i.lexeme orelse "" });
    }
}

// Tests

/// Test method to test parsing the input. Is essentially a wrapper function for simplicity, this should not be used necesarily
/// but is a good reference for future implementation
pub fn test_parse(input: []const u8, allocator: Allocator) !ArrayList(HtmlTag) {
    var tokenizer = Tokenizer{ .input = input };
    var tokens = try tokenizer.tokenize(allocator);

    defer tokens.deinit();

    var parser = Parser{ .tokens = tokens, .allocator = allocator };

    const tags = try parser.parse();
    return tags;
}
// TODO: other day
// test "parse comment tag" {
//     const input = "<!-- i am a comment -->";
//     var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
//     const allocator = arena.allocator();
//     defer arena.deinit();
//
//     var tokenizer = Tokenizer{ .input = input };
//     const tokens = try tokenizer.tokenize(allocator);
//     var parser = Parser{ .allocator = allocator, .tokens = tokens };
//     const tags = try parser.parse();
//
//     std.log.warn("tags: {}", .{tags.items[0]});
// }

// test "simple input tokens" {
//     const input = "<root></root>";
//
//     var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
//     const allocator = arena.allocator();
//
//     const tags = try test_parse(input, allocator);
//
//     const root = tags.items[0].name;
//     try std.testing.expect(std.mem.eql(u8, "root", root));
//
//     defer arena.deinit();
// }
//
// test "parse open tag with attribute and value" {
//     const input = "<root attr=\"attr_value\">test</root>";
//
//     var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
//     const allocator = arena.allocator();
//
//     var tokenizer = Tokenizer{ .input = input };
//     const tokens = try tokenizer.tokenize(allocator);
//
//     var parser = Parser{ .tokens = tokens, .allocator = allocator };
//
//     const tags = try parser.parse();
//
//     try std.testing.expectEqual(TokenType.OpeningTag, tokens.items[0].token_type);
//     try std.testing.expectEqualStrings("root", tokens.items[0].lexeme.?);
//     try std.testing.expectEqual(TokenType.AttributeName, tokens.items[1].token_type);
//     try std.testing.expectEqualStrings("attr", tokens.items[1].lexeme.?);
//     try std.testing.expectEqual(TokenType.AttributeValue, tokens.items[2].token_type);
//     try std.testing.expectEqualStrings("attr_value", tokens.items[2].lexeme.?);
//     try std.testing.expectEqual(TokenType.Value, tokens.items[3].token_type);
//     try std.testing.expectEqualStrings("test", tokens.items[3].lexeme.?);
//
//     const root = tags.items[0];
//     try std.testing.expectEqualStrings("root", root.name);
//     try std.testing.expectEqualStrings("attr", root.attrs.?[0].name);
//     try std.testing.expectEqualStrings("attr_value", root.attrs.?[0].value);
//     try std.testing.expectEqualStrings("test", root.value);
//
//     defer arena.deinit();
// }
//
// test "parse nested tags without attributes" {
//     const input = "<parent><child>child content</child></parent>";
//
//     var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
//     const allocator = arena.allocator();
//
//     var tokenizer = Tokenizer{ .input = input };
//     const tokens = try tokenizer.tokenize(allocator);
//
//     var parser = Parser{ .tokens = tokens, .allocator = allocator };
//
//     const tags = try parser.parse();
//
//     const parent = tags.items[0];
//     try std.testing.expectEqualStrings("parent", parent.name);
//     try std.testing.expect(parent.attrs == null);
//     try std.testing.expect(parent.children != null);
//     try std.testing.expect(parent.level == 1);
//
//     const child = parent.children.?[0];
//     try std.testing.expectEqualStrings("child", child.name);
//     try std.testing.expect(child.attrs == null);
//     try std.testing.expectEqualStrings("child content", child.value);
//     try std.testing.expect(child.level == 2);
//
//     defer arena.deinit();
// }
//
// test "parse double nested tags without attributes" {
//     const input = "<parent><child><grandchild>grandchild content</grandchild></child></parent>";
//
//     var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
//     const allocator = arena.allocator();
//
//     var tokenizer = Tokenizer{ .input = input };
//     const tokens = try tokenizer.tokenize(allocator);
//
//     var parser = Parser{ .tokens = tokens, .allocator = allocator };
//
//     const tags = try parser.parse();
//
//     const parent = tags.items[0];
//     try std.testing.expectEqualStrings("parent", parent.name);
//     try std.testing.expect(parent.attrs == null);
//     try std.testing.expect(parent.children != null);
//
//     const child = parent.children.?[0];
//     try std.testing.expectEqualStrings("child", child.name);
//     try std.testing.expect(child.attrs == null);
//
//     const grandchild = child.children.?[0];
//     try std.testing.expectEqualStrings("grandchild", grandchild.name);
//     try std.testing.expect(grandchild.attrs == null);
//     try std.testing.expectEqualStrings("grandchild content", grandchild.value);
//
//     defer arena.deinit();
// }
//
// test "parse nested tags with attributes" {
//     const input = "<parent parentAttr=\"parent\"><child>child content</child></parent>";
//     var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
//     const allocator = arena.allocator();
//
//     var tokenizer = Tokenizer{ .input = input };
//     const tokens = try tokenizer.tokenize(allocator);
//
//     var parser = Parser{ .tokens = tokens, .allocator = allocator };
//
//     const tags = try parser.parse();
//
//     const parent = tags.items[0];
//     try std.testing.expectEqualStrings("parent", parent.name);
//     try std.testing.expect(parent.attrs != null);
//     try std.testing.expectEqualStrings("parentAttr", parent.attrs.?[0].name);
//     try std.testing.expectEqualStrings("parent", parent.attrs.?[0].value);
//     try std.testing.expect(parent.children != null);
//
//     const child = parent.children.?[0];
//     try std.testing.expectEqualStrings("child", child.name);
//     try std.testing.expect(child.attrs == null);
//     try std.testing.expectEqualStrings("child content", child.value);
//
//     defer arena.deinit();
// }
//
// test "parse nested tags with attributes with child that has attrs" {
//     const input = "<parent parentAttr=\"parent\"><child childAttr=\"child\">child content</child></parent>";
//     var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
//     const allocator = arena.allocator();
//
//     var tokenizer = Tokenizer{ .input = input };
//     const tokens = try tokenizer.tokenize(allocator);
//
//     var parser = Parser{ .tokens = tokens, .allocator = allocator };
//
//     const tags = try parser.parse();
//
//     const parent = tags.items[0];
//     try std.testing.expectEqualStrings("parent", parent.name);
//     try std.testing.expect(parent.attrs != null);
//     try std.testing.expectEqualStrings("parentAttr", parent.attrs.?[0].name);
//     try std.testing.expectEqualStrings("parent", parent.attrs.?[0].value);
//     try std.testing.expect(parent.children != null);
//
//     const child = parent.children.?[0];
//     try std.testing.expectEqualStrings("child", child.name);
//     try std.testing.expectEqualStrings("childAttr", child.attrs.?[0].name);
//     try std.testing.expectEqualStrings("child", child.attrs.?[0].value);
//     try std.testing.expectEqualStrings("child content", child.value);
//
//     defer arena.deinit();
// }
//
// test "parse 10 nested children from a file." {
//     const ofile = try std.fs.cwd().openFile("test.xml", .{});
//     defer ofile.close();
//
//     const input = try ofile.readToEndAlloc(std.testing.allocator, 4096);
//     defer std.testing.allocator.free(input);
//
//     var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
//     const allocator = arena.allocator();
//
//     defer arena.deinit();
//
//     var tokenizer = Tokenizer{ .input = input };
//     const tokens = try tokenizer.tokenize(allocator);
//     var parser = Parser{ .tokens = tokens, .allocator = allocator };
//     const tags = try parser.parse();
//
//     const parent = tags.items[0];
//     const child = parent.children.?[0];
//     const grandchild = child.children.?[0];
//     try std.testing.expectEqualStrings("root", parent.name);
//     try std.testing.expectEqualStrings("child1", child.name);
//     try std.testing.expectEqualStrings("child2", grandchild.name);
// }

// HTML support
// need to figure out meta and style tags to support them
// need to also figure out why something is "InvalidToken" after img in this test case
test "parse html file" {
    const htmlFile = try std.fs.cwd().openFile("test.html", .{});
    defer htmlFile.close();

    const input = try htmlFile.readToEndAlloc(std.testing.allocator, 4096);
    defer std.testing.allocator.free(input);

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    const allocator = arena.allocator();

    defer arena.deinit();

    var tokenizer = Tokenizer{ .input = input };
    const tokens = try tokenizer.tokenize(allocator);

    // for (tokens.items) |item| {
    //     std.log.warn("token: {}", .{item.token_type});
    //     std.log.warn("lexeme: {s}", .{item.lexeme orelse "<null>"});
    // }

    var parser = Parser{ .allocator = allocator, .tokens = tokens };
    const tags = try parser.parse();

    _ = tags;
    //
    // for (tags.items) |tag| {
    //     std.log.warn("tag name: {s}", .{tag.name});
    //     std.log.warn("tag value: {s}", .{tag.value});
    // }
}

//this test is broken
//FIXME: need to add support for checking the : token I think in the xml
// test "parse from file" {
//     const ofile = try std.fs.cwd().openFile("sheet1.xml", .{});
//     defer ofile.close();
//
//     const input = try ofile.readToEndAlloc(std.testing.allocator, 4096);
//     defer std.testing.allocator.free(input);
//
//     var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
//     const allocator = arena.allocator();
//
//     defer arena.deinit();
//
//     var tokenizer = Tokenizer{ .input = input };
//     const tokens = try tokenizer.tokenize(allocator);
//     var parser = Parser{ .tokens = tokens, .allocator = allocator };
//     const tags = try parser.parse();
//
//     std.log.warn("tags:: {}", .{tags.items[0].level});
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
