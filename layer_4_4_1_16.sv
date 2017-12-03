module layer_4_4_1_16(clk, reset, s_valid, m_ready, data_in, m_valid, s_ready, data_out);
	parameter T = 16;
	input			clk, reset, s_valid, m_ready;
	input signed [15:0]	data_in;
	output logic signed [15:0]	data_out;
	output logic			m_valid, s_ready;

// for loop for data out and m valid

	logic signed [15:0] data_out1;
	logic signed [15:0] data_out2;
	logic m_valid1, m_valid2;
	logic [2:0] count1;

		mvma #(.rank(0)) mvma_4_16_1(clk, reset, s_valid, m_ready, data_in, m_valid1, s_ready, data_out1);
		mvma #(.rank(1)) mvma_4_16_2(clk, reset, s_valid, m_ready, data_in, m_valid2, s_ready, data_out2);


 assign m_valid = (m_valid1 || m_valid2) ;



 always_comb begin
     if ((reset == 1) || ((count1 == 2) && !(m_valid && m_ready)))
		     count1 = 0;
 	   if (m_valid && m_ready) begin
		    count1 = count1 + 1;
        if (count1 == 1)
		        data_out = data_out1;
		    else if (count1 == 2)
		        data_out = data_out2;
     end
  end



endmodule

module mvma (clk, reset, s_valid, m_ready, data_in, m_valid, s_ready, data_out);
	input			clk, reset, s_valid, m_ready;
	input signed [15:0]	data_in;
	output signed [15:0]	data_out;
	output			m_valid, s_ready;
	logic [3:0]		addr_w;
	logic [1:0]		addr_x;
	logic [1:0]		addr_b;
	logic			wr_en_x, accum_src, enable_accum;
	logic [2:0]		output_counter;

  parameter rank;

	control		#(.rank(rank))	ctrl(clk, reset, s_valid, m_ready, addr_x, wr_en_x, addr_w, accum_src, m_valid, s_ready, enable_accum, addr_b, output_counter);
	datapath	dpth(clk, reset, data_in, addr_x, wr_en_x, addr_w, accum_src, data_out, enable_accum, addr_b, output_counter);

endmodule

module control(clk, reset, s_valid, m_ready, addr_x, wr_en_x, addr_w, accum_src, m_valid, s_ready, accum_en, addr_b, output_counter);
	input			clk, reset, s_valid, m_ready;
	output logic [3:0]	addr_w;
	output logic [1:0]	addr_x;
	output logic [1:0]	addr_b;
	output logic		wr_en_x, accum_src, m_valid, s_ready, accum_en;
  logic [2:0] out_delay; // depends on rank -- or P
	logic [2:0] all_out; // depends on P

	logic			computing;
	logic [2:0]		input_counter;
	output logic [2:0]	output_counter;
  logic [2:0]  count, block; //depends on the number of inputs N
  parameter T=16, M=4, N=4, P=2, rank;


	always_comb begin
		if( (s_valid == 1) && (s_ready == 1) && (input_counter < 4) ) begin
			wr_en_x = 1;
			out_delay = 0;
			all_out = 0;
		end
		else
			wr_en_x = 0;

	  if ((m_valid == 1) && (m_ready == 1)) begin
					 out_delay <= out_delay + 1;
					 all_out <= all_out + 1;
		end
	  else if ( all_out == P)
							 all_out <= 0;


		if ( out_delay > (rank+1))
					 out_delay <= 0;

	end

	always_ff @(posedge clk) begin
		if(reset ==1) begin
			s_ready <= 1;
			addr_w <= rank*(N);
			addr_x <= 0;
			addr_b <= rank;
			accum_src <= 1;
			computing <= 0;
			input_counter <= 0;
			output_counter <= 0;
			accum_en <= 1;
			m_valid <= 0;
			block <= rank;
			count <= 0;
		end


		else if (computing == 0) begin
			if (s_valid == 1) begin
				if (input_counter < 4) begin
					if (addr_x == 3)
						addr_x <= 0;
					else
						addr_x <= addr_x + 1;
					input_counter <= input_counter + 1;
					if (input_counter == 3) begin
						input_counter <= 0;
						s_ready <= 0;
						computing <= 1;
					end
				end
			end
		end


		else if (computing == 1) begin
			if (all_out == P)
				 output_counter <= 0;
			else if (output_counter < 6)
				output_counter <= output_counter + 1;

			if ((m_valid == 1) && (m_ready == 1) && (all_out == P))
				accum_src <= 1;
			else if (output_counter == 0)
				accum_src <= 0;

			if (output_counter < 4) begin

				if (output_counter == 0) begin
				    addr_w <= block*(N) + 1;
						count <= count + 2;
        end
				else if (addr_w == (N*(M + rank - P + 1)) - 1)  /// whne out reaches maximum
					  addr_w <= rank*(N);

				else if(count > N-1) begin
					  count <= 0;
						block <= block + (M/P);
				end

				else begin
						addr_w <= block*(N) + count;
						count <= count + 1;
	       end

				if (addr_x == 3)
					addr_x <= 0;
				else
					addr_x <= addr_x + 1;
			end

			if (output_counter == 5) begin
				if (addr_b == (M*N)-(P-1-rank))
					addr_b <= rank;
				else
					addr_b <= addr_b + P;
					addr_w <= block*N;
			end

			if ((m_valid == 1) && (m_ready == 1) && (out_delay == rank + 1)) begin
				accum_en <= 1;
			end
			else if ((output_counter == 5) || (out_delay == rank + 2))
				accum_en <= 0;


			if ((m_valid == 1) && (m_ready == 1) && (all_out == P))
				m_valid <= 0;
			else if (output_counter == 5)
				m_valid <= 1;

      if (all_out == P)
			   m_valid <= 0;

			if ((m_valid == 1) && (m_ready == 1) && (all_out == P)) begin
				if (addr_w == block*N) begin
					computing <= 0;
					s_ready <= 1;
				end
			end

		end
	end
endmodule

module memory(clk, data_in, data_out, addr, wr_en);
	parameter WIDTH = 16, SIZE = 64, LOGSIZE = 6;
	input [WIDTH-1:0]		data_in;
	output logic [WIDTH-1:0]	data_out;
	input [LOGSIZE-1:0]		addr;
	input				clk, wr_en;
	logic [SIZE-1:0][WIDTH-1:0]	mem;
	always_ff @(posedge clk) begin
		data_out <= mem[addr];
		if (wr_en)
			mem[addr] <= data_in;
	end
endmodule

module layer_4_4_1_16_W_rom(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd40;
        1: z <= 16'd69;
        2: z <= 16'd108;
        3: z <= -16'd68;
        4: z <= 16'd68;
        5: z <= -16'd114;
        6: z <= 16'd58;
        7: z <= 16'd72;
        8: z <= 16'd22;
        9: z <= -16'd33;
        10: z <= -16'd38;
        11: z <= 16'd108;
        12: z <= -16'd50;
        13: z <= -16'd78;
        14: z <= -16'd3;
        15: z <= 16'd73;
      endcase
   end
endmodule

module layer_4_4_1_16_B_rom(clk, addr, z);
   input clk;
   input [1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd85;
        1: z <= 16'd16;
        2: z <= -16'd59;
        3: z <= 16'd48;
      endcase
   end
endmodule

module datapath(clk, reset, data_in, addr_x, wr_en_x, addr_w, accum_src, data_out, enable_accum, addr_b, output_counter);
	input				clk, reset, wr_en_x, accum_src, enable_accum;
	input signed [15:0]		data_in;
	input [3:0]			addr_w;
	input [1:0]			addr_x;
	input [1:0]			addr_b;
	input [2:0]			output_counter;

	output logic signed [15:0]	data_out;
	logic signed [15:0]		data_out_x, data_out_w, data_out_b;
	logic signed [15:0]		mult_out, add_out, mux_out, mult_out_temp;



	memory #(16, 4, 2)		mem_x(clk, data_in, data_out_x, addr_x, wr_en_x);
	layer_4_4_1_16_W_rom		rom_w(clk, addr_w, data_out_w);
	layer_4_4_1_16_B_rom		rom_b(clk, addr_b, data_out_b);

	assign mult_out_temp = (output_counter < 5) ? data_out_x * data_out_w : 0;

	always_comb begin

		if (accum_src == 1 || output_counter <= 1) begin
			add_out = 0;
			mux_out = data_out_b;

		end
		else begin
			add_out  = mult_out  + data_out;
			mux_out = add_out;
		end
	end

	always_ff @(posedge clk) begin
		mult_out <= mult_out_temp;
		if ((reset == 1) || (output_counter == 0)) begin
			data_out <= 0;
		end
		else if (enable_accum == 1) begin
			 if(output_counter == 6)  begin
   				if (mux_out > 0)
								data_out <= mux_out;
					else
								data_out <= 0;
			end
			else
				data_out <= mux_out;
		end
	end
endmodule



module tb_layer_4_4_1_16();

   parameter T = 16;
   parameter NUMINPUTVALS = 10000;
   parameter NUMOUTPUTVALS = 10000;
   parameter INFILENAME = "tb_layer_4_4_1_16.in";
   parameter EXPFILENAME = "tb_layer_4_4_1_16.exp";

   logic clk, s_valid, s_ready, m_valid, m_ready, reset;
   logic  [T-1:0] data_in;
   logic signed [T-1:0] data_out;

   logic signed [T-1:0] inValues [NUMINPUTVALS-1:0];
   logic signed [T-1:0] expValues [NUMOUTPUTVALS-1:0];
   logic s;

   initial clk=0;
   always #5 clk = ~clk;

   layer_4_4_1_16 dut(clk, reset, s_valid, m_ready, data_in, m_valid, s_ready, data_out);

   logic rb, rb2;
   always begin
      @(posedge clk);
      #1;
      s=std::randomize(rb, rb2);
   end

   logic [31:0] j;

   always @* begin
      if (s_valid == 1)
         data_in = inValues[j];
      else
         data_in = 'x;
   end

   always @* begin
      if ((j>=0) && (j<NUMINPUTVALS) && (rb==1'b1))
         s_valid=1;
      else
         s_valid=0;
   end

   always @(posedge clk) begin
      if (s_valid && s_ready)
         j <= #1 j+1;
   end

   logic [31:0] i;
   always @* begin
      if ((i>=0) && (i<NUMOUTPUTVALS) && (rb2==1'b1))
         m_ready = 1;
      else
         m_ready = 0;
   end

   integer errors = 0;

   always @(posedge clk) begin
      if (m_ready && m_valid) begin
         if (data_out !== expValues[i]) begin
            $display($time,,"ERROR: y[%d] = %x; expected value = %x", i, data_out, expValues[i]);
            errors = errors+1;
         end
         i=i+1;
      end
   end

   ////////////////////////////////////////////////////////////////////////////////

   initial begin
     $readmemb(INFILENAME, inValues);
     $readmemb(EXPFILENAME, expValues);

      j=0; i=0;

      // Before first clock edge, initialize
      m_ready = 0;
      reset = 0;

      // reset
      @(posedge clk); #1; reset = 1;
      @(posedge clk); #1; reset = 0;

      wait(i==NUMOUTPUTVALS);
      $display("Simulated %d outputs. Found %d errors.", NUMOUTPUTVALS, errors);
      $finish;
   end


endmodule
