module StdlibSuite_DivUS_1(
    input [31:0] io_x,
    input [31:0] io_y,
    output [32:0] io_z
);

  wire [32:0] T0;
  wire [32:0] T1;

  assign io_z = T0;
  assign T0 = $signed(T1) / $signed(io_y);
  assign T1 = {1'h0/* 0*/, io_x};
endmodule

