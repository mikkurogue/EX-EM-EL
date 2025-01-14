const std = @import("std");
const Allocator = std.mem.Allocator;

/// Row is responsible for allocation of the row and its cells.
/// Row must be deinitialized to free resources.
/// When we are looping over the input, and we create the row struct,
/// we need to do the necessary calculations/transformations
/// and then free the resources per row, instead of at the end of execution
/// this will prevent a stack overflow -> gc can never be fast enough
/// and gc will never free resources during execution.
pub const Row = struct {
    allocator: Allocator,
    cells: []Cell,

    /// initialize the row, memory allocated is the amount of cells required for the row.
    pub fn init(allocator: Allocator, cell_count: usize) Row {
        return Row{
            .allocator = allocator,
            .cells = try allocator.alloc(Cell, cell_count),
        };
    }

    /// Deinit the row, free the resources.
    pub fn deinit(self: *Row) void {
        try self.allocator.free(self.cells);
    }
};

/// the basic cell struct
/// this contains the name of the cell (like A1), the value of the cell and the metadata it needs.
/// Metadata is stuff like the position in the sheet etc. for edu purposes this is meager
pub const Cell = struct {
    metadata: CellMetadata,
    name: []const u8,
    value: CellValue,

    const Self = @This();

    /// intialize an empty cell struct
    /// Note the lack of deinit functionality here. we free the cell in its parent (Row)
    /// A cell by itself should not be initialized outside of a row unless we are trying to
    /// add a new cell or something to a row (impossible from this poc) so it will just
    /// take stack memory for no reason
    pub fn init(value: CellValue, metadata: CellMetadata, name: []const u8) Cell {
        return Cell{
            .metadata = metadata,
            .name = name,
            .value = value,
        };
    }
};

/// The various cell types that can exist. In excel this is the
/// data type of the cell, row or column essentially. this should be stored in cell metadata
/// so we can yoink it from there
pub const CellType = enum { text, number, float, date, boolean };

/// Cell value type.
pub const CellValue = union(enum) { text: []const u8, number: i32, float: f64, date: f64, boolean: bool };

/// The cell metadata.
/// The position in the sheet based on row x column
/// type being the cell type (if its a text, number, float, date, boolean)
pub const CellMetadata = struct {
    row: u32,
    column: u32,
    type: CellType,
    column_name: []const u8,
};

/// example for now.
pub fn transform_number_cell(raw_value: f64) f64 {
    return raw_value;
}

// example implementation fo tarnsform number
// fn main() void {
//
//     // intitialize a row with 4 cells
//     const row = Row.init(std.heap.GeneralPurposeAllocator(.{}){}, 4);
//     // defer the deiniti
//     defer row.deinit();
//
//     var i: usize = 0;
//     while (row.cells.len < i) : (i += 1) {
//         const curr = row.cells[i];
//         switch (curr.value) {
//             .number => |n| {
//                 const t = transform_number_cell(n);
//                 std.log.debug("number: {d}\n", .{t});
//             },
//         }
//     }
// }
