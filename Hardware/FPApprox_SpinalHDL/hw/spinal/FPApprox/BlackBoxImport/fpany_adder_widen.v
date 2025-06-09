module fpany_adder_widen #(
    parameter EXPO_WIDTH_MULT=3,    // Bias will still be following 2**(EXPO_WIDTH_MULT-1) - 1
    parameter MANT_WIDTH_MULT=4,
    parameter EXPO_WIDTH_PSUM=EXPO_WIDTH_MULT+3,
    parameter MANT_WIDTH_PSUM=MANT_WIDTH_MULT+1
)(
    input  wire                                     clk,
    input  wire                                     rst_n,
    input  wire [EXPO_WIDTH_MULT+MANT_WIDTH_MULT:0] a,         // mult result
    input  wire [EXPO_WIDTH_PSUM+MANT_WIDTH_PSUM:0] b,         // psum in
    output wire [EXPO_WIDTH_PSUM+MANT_WIDTH_PSUM:0] r          // psum out
);

    // * Zero Extend a to fit wider bits
    wire                       sign_a_ori = a[EXPO_WIDTH_MULT+MANT_WIDTH_MULT];
    wire [EXPO_WIDTH_MULT-1:0] expo_a_ori = a[EXPO_WIDTH_MULT+MANT_WIDTH_MULT-1:MANT_WIDTH_MULT];
    wire [MANT_WIDTH_MULT-1:0] mant_a_ori = a[MANT_WIDTH_MULT-1:0];

    wire [EXPO_WIDTH_PSUM+MANT_WIDTH_PSUM:0] a_ext = {sign_a_ori, {(EXPO_WIDTH_PSUM-EXPO_WIDTH_MULT){1'b0}}, expo_a_ori, mant_a_ori, {(MANT_WIDTH_PSUM-MANT_WIDTH_MULT){1'b0}}};

    // * Extracting FPAny
    wire                       sign_a = sign_a_ori;
    wire [EXPO_WIDTH_PSUM-1:0] expo_a = a_ext[EXPO_WIDTH_PSUM+MANT_WIDTH_PSUM-1:MANT_WIDTH_PSUM];

    wire                       sign_b = b[EXPO_WIDTH_PSUM+MANT_WIDTH_PSUM];
    wire [EXPO_WIDTH_PSUM-1:0] expo_b = b[EXPO_WIDTH_PSUM+MANT_WIDTH_PSUM-1:MANT_WIDTH_PSUM];

    // * Find which FP has bigger Exponent 
    wire expo_a_larger = (expo_a > expo_b);
    wire [EXPO_WIDTH_PSUM+MANT_WIDTH_PSUM:0] fp_expo_bg = expo_a_larger ? a_ext : b    ;
    wire [EXPO_WIDTH_PSUM+MANT_WIDTH_PSUM:0] fp_expo_sm = expo_a_larger ? b     : a_ext;

    // * Exponent align
    wire [EXPO_WIDTH_PSUM-1:0] expo_bg = fp_expo_bg[EXPO_WIDTH_PSUM+MANT_WIDTH_PSUM-1:MANT_WIDTH_PSUM];
    wire [EXPO_WIDTH_PSUM-1:0] expo_sm = fp_expo_sm[EXPO_WIDTH_PSUM+MANT_WIDTH_PSUM-1:MANT_WIDTH_PSUM];
    wire [EXPO_WIDTH_PSUM-1:0] expo_diff_abs = expo_bg - expo_sm;
    wire [EXPO_WIDTH_PSUM-1:0] expo_choose   = expo_bg;

    // * Extracting FPAny (bigger & smaller)
    wire [MANT_WIDTH_PSUM-1:0] mant_bg = fp_expo_bg[MANT_WIDTH_PSUM-1:0];
    wire [MANT_WIDTH_PSUM-1:0] mant_sm = fp_expo_sm[MANT_WIDTH_PSUM-1:0];

    wire sign_bg = fp_expo_bg[EXPO_WIDTH_PSUM+MANT_WIDTH_PSUM];
    wire sign_sm = fp_expo_sm[EXPO_WIDTH_PSUM+MANT_WIDTH_PSUM];

    // * Mantissa align (at least one mant will not need to move)
    wire [MANT_WIDTH_PSUM+1:0] aligned_mant_bg = {1'b1, mant_bg, 1'b0};
    wire [MANT_WIDTH_PSUM+1:0] aligned_mant_sm = {1'b1, mant_sm, 1'b0} >> expo_diff_abs;

    // * Mantissa calculate
    wire [MANT_WIDTH_PSUM+2:0] mant_add = aligned_mant_bg + aligned_mant_sm;
    wire [MANT_WIDTH_PSUM+2:0] mant_min = aligned_mant_bg - aligned_mant_sm;
    wire [MANT_WIDTH_PSUM+2:0] mant_sum = (sign_a == sign_b) ? mant_add : mant_min;

    wire final_sign = (sign_a == sign_b) ? sign_a : sign_bg;

    // * Normalize
    wire [EXPO_WIDTH_PSUM-1:0] exp_norm  = mant_sum[MANT_WIDTH_PSUM+2] ? (expo_choose + 1'b1) :             // mant_sum[MANT_WIDTH_PSUM+2] means there is carry in at Mantissa's calculation
                                           mant_sum[MANT_WIDTH_PSUM+1] ?  expo_choose         :             // mant_sum[MANT_WIDTH_PSUM+1] is the original place for that hidden 1'b1 
                                                                         (expo_choose - 1'b1) ;
    wire [MANT_WIDTH_PSUM+1:0] mant_norm = mant_sum[MANT_WIDTH_PSUM+2] ? mant_sum[MANT_WIDTH_PSUM+2:1]      :    // mant_sum[MANT_WIDTH_PSUM+2] means there is carry in at Mantissa's calculation
                                           mant_sum[MANT_WIDTH_PSUM+1] ? mant_sum[MANT_WIDTH_PSUM+1:0]      :    // mant_sum[MANT_WIDTH_PSUM+1] is the original place for that hidden 1'b1 
                                                                        {mant_sum[MANT_WIDTH_PSUM:0], 1'b0} ;
    
    wire [MANT_WIDTH_PSUM:0] mant_round = (mant_norm[0]) ? mant_norm[MANT_WIDTH_PSUM:0] + 1'b1 : mant_norm[MANT_WIDTH_PSUM:0];    // Rounding up if the first bit is 1

    // * Assemble the final result
    assign r = {final_sign, exp_norm, mant_round[MANT_WIDTH_PSUM:1]};

endmodule


