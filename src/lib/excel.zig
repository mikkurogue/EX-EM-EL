const std = @import("std");
const zip = std.zip;
const fs = std.fs;

/// unzip and extract the excel file and its contents
/// The contents of the excel file follow the OPC standard found
/// here: https://en.wikipedia.org/wiki/Open_Packaging_Conventions
pub fn extract_excel(file_name: []const u8) !void {
    const file = try fs.cwd().openFile(file_name, .{});
    defer file.close();

    try fs.cwd().makeDir("temp");

    var temp_dir = try fs.cwd().openDir("temp", .{});
    defer temp_dir.close();

    const seekable_stream = file.seekableStream();

    try zip.extract(temp_dir, seekable_stream, .{});
}

/// Collect the relevant files and start parsing them
/// TODO: this needs to be implemented after the Lexer works.
/// We also need to find the relevant XML files that consist
/// of the data and the columns and rows for the relevant sheets.
fn collect_relevant_xml_files() !void {
    var temp = try fs.cwd().openDir("temp", .{});
    defer temp.close();

    std.log.debug("temp dir {any}", .{temp});
}

/// helper fn to delete the temp directory
fn remove_temp() !void {
    try std.fs.cwd().deleteTree("temp");
}
