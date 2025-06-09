// MARK: This is the simplified verilog version of the IP ternary_adder_xilinx.vhd

module ternary_adder_noincr(
    input  wire [7:0] x,
    input  wire [7:0] y,
    input  wire [7:0] z,
    input  wire       cin,
    output wire [7:0] sum
);

    wire [7:0] fa_c;           // Carry of each FullAdder
    wire [7:0] cms;            // Carry Mux Select for carry4

    genvar i;
    generate
        for (i = 0; i < 8; i = i + 1) begin : LUT6_2_block
            LUT6_2 #(
                .INIT  (64'h3cc3c33cfcc0fcc0        )         // Specify LUT Contents
            ) LUT6_2_inst (
                .O6    (cms[i]                      ),        // cms(n)
                .O5    (fa_c[i]                     ),        // c'(n)
                .I0    (1'b0                        ),        // gnd
                .I1    (z[i]                        ),        // z(n)
                .I2    (y[i]                        ),        // y(n)
                .I3    (x[i]                        ),        // x(n)
                .I4    ((i == 0) ? 1'b0 : fa_c[i-1] ),        // c'(n-1)
                .I5    (1'b1                        )         // vdd (fast MUX select only available to O6 output)
            );
        end
    endgenerate


    CARRY8 #(
        .CARRY_TYPE    ("SINGLE_CY8"       )         // 8-bit or dual 4-bit carry (DUAL_CY4, SINGLE_CY8)
    ) CARRY8_inst (
        .CO            (                   ),        // 8-bit output: Carry-out
        .O             (sum                ),        // 8-bit output: Carry chain XOR data out
        .CI            (cin                ),        // 1-bit input: Lower Carry-In
        .CI_TOP        (1'b0               ),        // 1-bit input: Upper Carry-In (Tie to ground if CARRY_TYPE=SINGLE_CY8)
        .DI            ({fa_c[6:0], 1'b0}  ),        // 8-bit input: Carry-MUX data in
        .S             (cms                )         // 8-bit input: Carry-mux select
    );



endmodule