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

    /// Advance to the next token - returns the character
    fn next(self: *Tokenizer) u8 {
        const result = self.input[self.current];
        self.current += 1;
        return result;
    }

    /// peek the next token, but seeing what the next token will be and return the character
    fn peek(self: *Tokenizer) u8 {
        return self.input[self.current];
    }

    /// check if we are at the end of the input
    /// returns a boolean
    fn end(self: *Tokenizer) bool {
        return self.current >= self.input.len;
    }

    /// TODO: add support for \n as a token, same prob for \r
    pub fn tokenize(self: *Tokenizer, allocator: Allocator) anyerror!ArrayList(Token) {
        var output = ArrayList(Token).init(allocator);

        while (!self.end()) {
            const char: u8 = self.next();

            const token: Token = switch (char) {
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
                            .token_type = TokenType.ClosingTag,
                            .lexeme = tag_name,
                        };
                    } else {
                        // handle opening tags
                        const start = self.current;
                        while (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '-') {
                            _ = self.next();
                        }
                        const tag_name = self.input[start..self.current];
                        try output.append(Token{ .token_type = TokenType.OpeningTag, .lexeme = tag_name });

                        // parse attributes
                        while (!self.end()) {
                            // skip whitespace
                            while (std.ascii.isWhitespace(self.peek())) _ = self.next();

                            if (self.peek() == '/') {
                                _ = self.next();
                                _ = self.next();
                                break :openTag Token{
                                    .token_type = TokenType.SelfClosingTag,
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
                                        .token_type = TokenType.AttributeName,
                                        .lexeme = attr_name,
                                    });
                                    try output.append(Token{
                                        .token_type = TokenType.AttributeValue,
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
                            .token_type = TokenType.SelfClosingTag,
                        };
                    }
                    break :slashTag Token{ .token_type = TokenType.Slash };
                },
                '=' => Token{ .token_type = TokenType.Equal },
                '!' => Token{ .token_type = TokenType.Bang },

                else => valueBlock: {
                    // handle text values
                    const start = self.current - 1;
                    while (!self.end() and self.peek() != '<') {
                        _ = self.next();
                    }
                    break :valueBlock Token{
                        .token_type = TokenType.Value,
                        .lexeme = self.input[start..self.current],
                    };
                },
            };

            try output.append(token);
            self.start = self.current;
        }

        // Append EOF only after finishing the tokenization process
        try output.append(.{ .token_type = TokenType.EOF, .lexeme = "<EOF>" });
        return output;
    }
};

/// Potential errors the Parser can throw
pub const ParserError = error{ MismatchedClosingTag, InvalidAttribute, UnexpectedEOF, UnexpectedClosingTag, UnexpectedToken, OutOfMemory, InvalidToken, NoTagName };
/// The diagnostics for the parser for feedback during iteration
pub const ParseDiagnostics = struct {
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

    /// TODO: add line counter for when we encounter \n
    fn parse_tag(self: *Parser) ParserError!HtmlTag {
        const opening_token = self.next();
        if (opening_token.token_type != TokenType.OpeningTag) {
            std.log.warn("opening token {any}", .{opening_token.token_type});
            return ParserError.InvalidToken;
        }

        const tag_name = opening_token.lexeme orelse "";

        var attrs = ArrayList(HtmlAttr).init(self.allocator);

        while (self.peek().token_type == TokenType.AttributeName) {
            const attr_name = self.next().lexeme orelse "";

            if (self.peek().token_type != TokenType.AttributeValue) {
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
        while (!self.end() and self.peek().token_type != TokenType.ClosingTag) {
            if (self.peek().token_type == TokenType.OpeningTag) {
                try children.append(try self.parse_tag());
            } else if (self.peek().token_type == TokenType.Value) {
                value = self.next().lexeme orelse "";
            } else {
                attrs.deinit();
                children.deinit();
                return ParserError.InvalidToken;
            }
        }

        const closing_token = self.next();
        if (closing_token.token_type != TokenType.ClosingTag or
            !std.mem.eql(u8, tag_name, closing_token.lexeme orelse ""))
        {
            attrs.deinit();
            children.deinit();
            return ParserError.MismatchedClosingTag;
        }

        return HtmlTag{
            .name = tag_name,
            .attrs = if (attrs.items.len > 0) try attrs.toOwnedSlice() else null,
            .value = value,
            .children = if (children.items.len > 0) try children.toOwnedSlice() else null,
        };
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

const HtmlTag = struct { name: []const u8, attrs: ?[]HtmlAttr, value: []const u8, children: ?[]HtmlTag };
const HtmlAttr = struct {
    name: []const u8,
    value: []const u8,
};

/// Test method to map the tokens to the lexemes returned, remember that lexemes are nullable so if its an empty string
/// it means the lexeme has no value to be mapped to a token so it will stay empty
pub fn printTokens(tokens: *const std.ArrayList(Token)) !void {
    for (tokens.items) |i| {
        std.debug.print("{s}: '{s}'\n", .{ switch (i.token_type) {
            TokenType.OpeningTag => "OpeningTag",
            TokenType.ClosingTag => "ClosingTag",
            TokenType.SelfClosingTag => "SelfClosingTag",
            TokenType.Value => "Value",
            TokenType.TagName => "TagName",
            TokenType.Slash => "Slash",
            TokenType.Equal => "Equal",
            TokenType.Bang => "Bang",
            TokenType.EOF => "<EOF>",
            TokenType.AttributeValue => "Attribute Value",
            TokenType.AttributeName => "Attribute name",
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
    var parse_diag: ParseDiagnostics = ParseDiagnostics{ .line = 0, .token = Token{ .lexeme = null, .token_type = TokenType.EOF } };

    const tags = try parser.parse(&parse_diag);
    return tags;
}

test "simple input tokens" {
    const input = "<root></root>";

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    const allocator = arena.allocator();

    const tags = try test_parse(input, allocator);

    const root = tags.items[0].name;
    try std.testing.expect(std.mem.eql(u8, "root", root));

    defer arena.deinit();
}

test "parse open tag with attribute and value" {
    const input = "<root attr=\"attr_value\">test</root>";

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    const allocator = arena.allocator();

    var tokenizer = Tokenizer{ .input = input };
    const tokens = try tokenizer.tokenize(allocator);

    var parser = Parser{ .tokens = tokens, .allocator = allocator };
    var parse_diag = ParseDiagnostics{ .line = 0, .token = null };

    const tags = try parser.parse(&parse_diag);

    try std.testing.expectEqual(TokenType.OpeningTag, tokens.items[0].token_type);
    try std.testing.expectEqualStrings("root", tokens.items[0].lexeme.?);
    try std.testing.expectEqual(TokenType.AttributeName, tokens.items[1].token_type);
    try std.testing.expectEqualStrings("attr", tokens.items[1].lexeme.?);
    try std.testing.expectEqual(TokenType.AttributeValue, tokens.items[2].token_type);
    try std.testing.expectEqualStrings("attr_value", tokens.items[2].lexeme.?);
    try std.testing.expectEqual(TokenType.Value, tokens.items[3].token_type);
    try std.testing.expectEqualStrings("test", tokens.items[3].lexeme.?);

    const root = tags.items[0];
    try std.testing.expectEqualStrings("root", root.name);
    try std.testing.expectEqualStrings("attr", root.attrs.?[0].name);
    try std.testing.expectEqualStrings("attr_value", root.attrs.?[0].value);
    try std.testing.expectEqualStrings("test", root.value);

    defer arena.deinit();
}

test "parse nested tags without attributes" {
    const input = "<parent><child>child content</child></parent>";

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    const allocator = arena.allocator();

    var tokenizer = Tokenizer{ .input = input };
    const tokens = try tokenizer.tokenize(allocator);

    var parser = Parser{ .tokens = tokens, .allocator = allocator };
    var parse_diag = ParseDiagnostics{ .line = 0, .token = null };

    const tags = try parser.parse(&parse_diag);
    const parent = tags.items[0];
    try std.testing.expectEqualStrings("parent", parent.name);
    try std.testing.expect(parent.attrs == null);
    try std.testing.expect(parent.children != null);

    const child = parent.children.?[0];
    try std.testing.expectEqualStrings("child", child.name);
    try std.testing.expect(child.attrs == null);
    try std.testing.expectEqualStrings("child content", child.value);

    defer arena.deinit();
}

test "parse double nested tags without attributes" {
    const input = "<parent><child><grandchild>grandchild content</grandchild></child></parent>";

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    const allocator = arena.allocator();

    var tokenizer = Tokenizer{ .input = input };
    const tokens = try tokenizer.tokenize(allocator);

    var parser = Parser{ .tokens = tokens, .allocator = allocator };
    var parse_diag = ParseDiagnostics{ .line = 0, .token = null };

    const tags = try parser.parse(&parse_diag);

    const parent = tags.items[0];
    try std.testing.expectEqualStrings("parent", parent.name);
    try std.testing.expect(parent.attrs == null);
    try std.testing.expect(parent.children != null);

    const child = parent.children.?[0];
    try std.testing.expectEqualStrings("child", child.name);
    try std.testing.expect(child.attrs == null);

    const grandchild = child.children.?[0];
    try std.testing.expectEqualStrings("grandchild", grandchild.name);
    try std.testing.expect(grandchild.attrs == null);
    try std.testing.expectEqualStrings("grandchild content", grandchild.value);

    defer arena.deinit();
}

test "parse nested tags with attributes" {
    const input = "<parent parentAttr=\"parent\"><child>child content</child></parent>";
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    const allocator = arena.allocator();

    var tokenizer = Tokenizer{ .input = input };
    const tokens = try tokenizer.tokenize(allocator);

    var parser = Parser{ .tokens = tokens, .allocator = allocator };
    var parse_diag = ParseDiagnostics{ .line = 0, .token = null };

    const tags = try parser.parse(&parse_diag);

    const parent = tags.items[0];
    try std.testing.expectEqualStrings("parent", parent.name);
    try std.testing.expect(parent.attrs != null);
    try std.testing.expectEqualStrings("parentAttr", parent.attrs.?[0].name);
    try std.testing.expectEqualStrings("parent", parent.attrs.?[0].value);
    try std.testing.expect(parent.children != null);

    const child = parent.children.?[0];
    try std.testing.expectEqualStrings("child", child.name);
    try std.testing.expect(child.attrs == null);
    try std.testing.expectEqualStrings("child content", child.value);

    defer arena.deinit();
}

test "parse nested tags with attributes with child that has attrs" {
    const input = "<parent parentAttr=\"parent\"><child childAttr=\"child\">child content</child></parent>";
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    const allocator = arena.allocator();

    var tokenizer = Tokenizer{ .input = input };
    const tokens = try tokenizer.tokenize(allocator);

    var parser = Parser{ .tokens = tokens, .allocator = allocator };
    var parse_diag = ParseDiagnostics{ .line = 0, .token = null };

    const tags = try parser.parse(&parse_diag);

    const parent = tags.items[0];
    try std.testing.expectEqualStrings("parent", parent.name);
    try std.testing.expect(parent.attrs != null);
    try std.testing.expectEqualStrings("parentAttr", parent.attrs.?[0].name);
    try std.testing.expectEqualStrings("parent", parent.attrs.?[0].value);
    try std.testing.expect(parent.children != null);

    const child = parent.children.?[0];
    try std.testing.expectEqualStrings("child", child.name);
    try std.testing.expectEqualStrings("childAttr", child.attrs.?[0].name);
    try std.testing.expectEqualStrings("child", child.attrs.?[0].value);
    try std.testing.expectEqualStrings("child content", child.value);

    defer arena.deinit();
}

test "parse 10 nested children" {
    const input =
        \\\<root>
        \\\    <child1>
        \\\     <child2>
        \\\           <child3>
        \\\               <child4>
        \\\                   <child5>
        \\\                      <child6>
        \\\                           <child7>
        \\\                               <child8>
        \\\                                    <child9>
        \\\                                        <child10>
        \\\                                     </child10>
        \\\                              </child9>
        \\\                             </child8>
        \\\                         </child7>
        \\\                     </child6>
        \\\                  </child5>
        \\\                </child4>
        \\\            </child3>
        \\\        </child2>
        \\\    </child1>
        \\\</root>
    ;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    const allocator = arena.allocator();

    defer arena.deinit();

    var tokenizer = Tokenizer{ .input = input };
    const tokens = try tokenizer.tokenize(allocator);
    var parser = Parser{ .tokens = tokens, .allocator = allocator };
    var parse_diag = ParseDiagnostics{ .line = 0, .token = null };
    const tags = try parser.parse(&parse_diag);

    const parent = tags.items[0];
    try std.testing.expectEqualStrings("root", parent.name);
}

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
