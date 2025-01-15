const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ascii = std.ascii;
const eql = std.mem.eql;

const TokenType = enum {
    LeftAngleBracket,
    RightAngleBracket,
    Slash,
    Equal,
    String,
    Bang,
    Text,
    Unknown,
    EOF,
};

const Token = struct {
    lexeme: ?[]const u8 = null,
    token_type: TokenType,
};

const Scanner = struct {
    current: usize = 0,
    start: usize = 0,
    input: []const u8,

    fn advance(self: *Scanner) u8 {
        const result = self.input[self.current];
        self.current += 1;
        return result;
    }

    fn peek(self: *Scanner) u8 {
        return self.input[self.current];
    }

    fn end(self: *Scanner) bool {
        return self.current >= self.input.len;
    }

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

                    break :blk Token{ .lexeme = self.input[self.start..self.current], .token_type = TokenType.String };
                },
                else => blk: {
                    while (ascii.isAlphanumeric(self.peek()) or self.peek() == '-') {
                        _ = self.advance();
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

const ParserError = error{
    UnexpectedEOF,
    UnexpectedClosingTag,
    UnexpectedToken,
};

const ParseDiagnostics = struct {
    line: usize,
    token: ?Token,
};

const Parser = struct {
    allocator: Allocator,
    tokens: ArrayList(Token),
    current: usize = 0,
    current_tag: ?[]const u8 = null,
    line: usize = 0,

    fn end(self: *Parser) bool {
        return self.current >= self.tokens.items.len;
    }

    fn peek(self: *Parser) Token {
        if (self.end()) return self.tokens.items[self.current - 1];
        return self.tokens.items[self.current];
    }

    fn advance(self: *Parser) Token {
        if (self.end()) return self.tokens.items[self.current - 1];

        const tmp = self.tokens.items[self.current];
        self.current += 1;
        return tmp;
    }

    fn check(self: *Parser, token_type: TokenType) bool {
        if (self.end()) return false;
        return self.peek().token_type == token_type;
    }

    fn match(self: *Parser, token_type: TokenType) ParserError!Token {
        if (self.check(token_type)) {
            return self.advance();
        }
        return ParserError.UnexpectedToken;
    }

    // FIXME: this breaks somehowon the first test.
    fn parse_tag(self: *Parser) ParserError!HtmlTag {
        _ = try self.match(TokenType.LeftAngleBracket);

        const tag_name = (try self.match(TokenType.Text)).lexeme orelse "";
        const attrs: ?[]const HtmlAttr = try self.parse_attrs();
        const value: []const u8 = try self.parse_value();

        _ = try self.match(TokenType.LeftAngleBracket);
        const close_tag_name = (try self.match(TokenType.Text)).lexeme orelse "";
        _ = try self.match(TokenType.RightAngleBracket);

        if (eql(u8, tag_name, close_tag_name)) {
            std.log.warn("open and close tags do not match: {s} and {s}", .{ tag_name, close_tag_name });
            return ParserError.UnexpectedClosingTag;
        }

        return HtmlTag{ .name = tag_name, .attrs = attrs, .value = value };
    }

    // FIXME: what the fuck is this
    fn parse_attrs(_: *Parser) !?[]const HtmlAttr {
        return null;
    }

    // FIXME: Also what the fuck is this
    fn parse_value(_: *Parser) ![]const u8 {
        return "";
    }

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

const HtmlTag = struct {
    name: []const u8,
    attrs: ?[]const HtmlAttr,
    value: []const u8,
};

const HtmlAttr = struct {
    name: []const u8,
    value: []const u8,
};

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
        }, i.lexeme orelse "" });
    }
}

// Tests

fn test_parse(input: []const u8, allocator: Allocator) !ArrayList(HtmlTag) {
    var scanner = Scanner{ .input = input };
    var tokens = try scanner.scan_tokens(allocator);

    defer tokens.deinit();

    var parser = Parser{ .tokens = tokens, .allocator = allocator };
    var parse_diag: ParseDiagnostics = ParseDiagnostics{ .line = 0, .token = Token{ .lexeme = null, .token_type = TokenType.EOF } };

    const tags = try parser.parse(&parse_diag);
    return tags;
}

test "simple input" {
    const input = "<hello>";
    var sc = Scanner{ .input = input };
    const t = try sc.scan_tokens(testing.allocator);
    defer t.deinit();

    try printTokens(&t);
}

// test "parse open tag without attributes" {
//     const tags = try test_parse("<hello></hello>", std.testing.allocator);
//     defer tags.deinit();
//     const root = tags.items[0];
//
//     std.log.warn("root: {any}", .{root});
//
//     // try std.testing.expect(std.mem.eql(u8, root.name, "hello"));
// }

// test "scanner tokenizes basic input" {
//     var scanner = Scanner{ .input = "<hello></hello>" };
//     const tokens = try scanner.scan_tokens(std.testing.allocator);
//     defer tokens.deinit();
//
//     for (tokens.items) |token| {
//         std.log.warn("Token: {s}, Type: {any}", .{
//             token.lexeme orelse "<null>",
//             token.token_type,
//         });
//     }
//
//     try std.testing.expectEqual(tokens.items.len, 8);
// }
//
// test "scanner tokenizes basic input" {
//     var scanner = Scanner{ .input = "<hello></hello>" };
//     const tokens = try scanner.scan_tokens(std.testing.allocator);
//     defer tokens.deinit();
//
//     var i: usize = 0;
//     while (i < tokens.items.len) : (i += 1) {
//         std.log.warn("token: {s}", .{tokens.items[i].lexeme.?});
//     }
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
