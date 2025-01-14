const std = @import("std");
const Allocator = std.mem.Allocator;
const zip = std.zip;

pub const Row = struct {
    allocator: Allocator,
    name: []const u8,
    cells: []Cell,
    cell_count: usize,

    pub fn init(allocator: Allocator, name: []const u8, cell_count: usize) Row {
        return Row{
            .allocator = allocator,
            .name = name,
            .cells = try allocator.alloc(Cell, cell_count),
            .cell_count = cell_count,
        };
    }

    pub fn deinit(self: *Row) void {
        try self.allocator.free(self.cells);
    }
};

pub const Cell = struct {
    value: []const u8,

    pub fn init(value: []const u8) Cell {
        return Cell{ .value = value };
    }
};

pub fn main() !void {
    const file = try std.fs.cwd().openFile("test.xlsx", .{});
    defer file.close();

    try std.fs.cwd().makeDir("temp");

    var temp_dir = try std.fs.cwd().openDir("temp", .{});
    defer temp_dir.close();

    const seekable_stream = file.seekableStream();

    try std.zip.extract(temp_dir, seekable_stream, .{});
}

// pub fn main() !void {
//     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//     defer _ = gpa.deinit();
//     const allocator = gpa.allocator();
//
//     const file = try std.fs.cwd().openFile("test.csv", .{});
//     defer file.close();
//
//     var buf_reader = std.io.bufferedReader(file.reader());
//     var in_stream = buf_reader.reader();
//
//     while (try in_stream.readUntilDelimiterOrEofAlloc(allocator, '\n', 8192)) |line| {
//         // do something with line...
//         // std.log.debug("{s}", .{line});
//
//         var iter = std.mem.splitSequence(u8, line, ",");
//
//         while (iter.next()) |val| {
//             var row = Row.init(allocator, val, iter.rest().len);
//             defer row.deinit();
//             // std.log.debug("{s}", .{val});
//         }
//
//         defer {
//             allocator.free(line);
//         }
//     }
// }

// pub const ExcelSheet = struct {
//     name: []const u8,
//     rows: [][]ExcelCell,
//
//     pub const ExcelCell = union(enum) {
//         text: []const u8,
//         number: f64,
//         empty: void,
//     };
// };
//
// pub const ExcelFile = struct {
//     filename: []const u8,
//     sheets: []ExcelSheet,
//
//     pub fn deinit(self: *ExcelFile, allocator: Allocator) void {
//         for (self.sheets) |sheet| {
//             for (sheet.rows) |row| {
//                 for (row) |cell| {
//                     switch (cell) {
//                         .text => |t| allocator.free(t),
//                         else => {},
//                     }
//                 }
//                 allocator.free(row);
//             }
//             allocator.free(sheet.name);
//             allocator.free(sheet.rows);
//         }
//
//         allocator.free(self.filename);
//         allocator.free(self.sheets);
//     }
// };
//
// pub fn readExcelFile(allocator: Allocator, path: []const u8) !ExcelFile {
//     const file = try std.fs.cwd().openFile(path,.{});
//     defer file.close();
//
//     // TODO: unzip the excel file here and create the struct
//
// }
