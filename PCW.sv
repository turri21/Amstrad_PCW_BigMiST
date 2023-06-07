
module PCW
(
	//Master input clock
	input         CLOCK_27,

	
	

	output  [5:0] VGA_R,
	output  [5:0] VGA_G,
	output  [5:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,
	
	output        LED,  // 1 - ON, 0 - OFF.

	
	output        AUDIO_L,
	output        AUDIO_R,
	
	//SDRAM interface with lower latency
	output        SDRAM_CLK,
	output        SDRAM_CKE,
	output [12:0] SDRAM_A,
	output  [1:0] SDRAM_BA,
	inout  [15:0] SDRAM_DQ,
	output        SDRAM_DQML,
	output        SDRAM_DQMH,
	output        SDRAM_nCS,
	output        SDRAM_nCAS,
	output        SDRAM_nRAS,
	output        SDRAM_nWE,

   input         SPI_SCK,
   output        SPI_DO,
   input         SPI_DI,
   input         SPI_SS2,
   input         SPI_SS3,
   input         CONF_DATA0,

	input         UART_RX,
	output        UART_TX,
	output [9:0]  DAC_L,
	output [9:0]  DAC_R
	);

	


assign {UART_TX} = 0;
//assign {SD_SCK, SD_MOSI, SD_CS} = 'Z;
//assign USER_OUT = '1;
//assign {SDRAM_DQ, SDRAM_A, SDRAM_BA, SDRAM_CLK, SDRAM_CKE, SDRAM_DQML, SDRAM_DQMH, SDRAM_nWE, SDRAM_nCAS, SDRAM_nRAS, SDRAM_nCS} = 'Z;



//assign LED  = ioctl_download;

localparam BOOT_ROM_END = 16'd275;	// Length of boot rom

`include "build_id.v"
localparam CONF_STR = {
	"PCW;;",
	"S0,DSK,Mount A:;",
	"S1,DSK,Mount B:;",
	"O4,System Model,8256/8512,9256/9512+;",
	"OFG,Memory Size,256K,512K,1MB,2MB;",
	"O89,Clockspeed Mhz,4,8,16,32;",
	"O56,Screen Color,White,Green,Amber;",
	"OIJ,Fake Colour,None,Palette CGA, Palette EGA;",
	"O7,Video System,PAL,NTSC;",
	"O13,Scandoubler Fx,None,HQ2x,CRT 25%,CRT 50%,CRT 75%;",
	"OAC,Joystick Type,None,Kempston,Spectravideo,Cascade,DKTronics;",
	"ODE,Mouse Type,None,AMX,Kempston,Keymouse;",
	"OH,DKTronics I/F,Disabled,Enabled;",
	"T0,Reset;",
	"V,v",`BUILD_DATE
};

(* preserve *) wire clk_sys;
wire locked;
pll pll
(
	.inclk0   (CLOCK_27),
	.c0 (clk_sys), // 64 MHz
	.locked (locked)
);

wire [31:0] status;
wire  [1:0] buttons;
wire [31:0] sd_lba;
wire  [1:0] sd_rd;
wire  [1:0] sd_wr;
wire        sd_ack;
wire  [8:0] sd_buff_addr;
wire  [7:0] sd_buff_dout;
wire  [7:0] sd_buff_din;
wire        sd_buff_wr;
wire  [1:0] img_mounted;
wire        img_readonly;
wire [63:0] img_size;

wire        forced_scandoubler;
wire [10:0] ps2_key;
wire [24:0] ps2_mouse;

wire [21:0] gamma_bus;

wire [15:0] joystick_0, joystick_1;


mist_io #(.STRLEN(($size(CONF_STR)>>3) )) mist_io
(
	.clk_sys(clk_sys),

	.conf_str(CONF_STR),

	.SPI_SCK   (SPI_SCK),
   .CONF_DATA0(CONF_DATA0),
   .SPI_SS2   (SPI_SS2),
   .SPI_DO    (SPI_DO),
   .SPI_DI    (SPI_DI),


	.ps2_key(ps2_key),
	.ps2_mouse(ps2_mouse),

	.joystick_0(joystick_0),
	.joystick_1(joystick_1),
	.buttons(buttons),
	
	.status(status),

	.sd_lba(sd_lba),
	.sd_rd(sd_rd),
	.sd_wr(sd_wr),
	.sd_ack(sd_ack),
	.sd_buff_addr(sd_buff_addr),
	.sd_buff_dout(sd_buff_dout),
	.sd_buff_din(sd_buff_din),
	.sd_buff_wr(sd_buff_wr),

	.img_mounted(img_mounted),
	.img_size(img_size)
);

wire reset = ~locked | status[0];

// signals from loader
logic loader_wr;		
logic loader_download;
reg [15:0] loader_addr;
reg [7:0] loader_data;
reg [15:0] execute_addr;
logic execute_enable;
logic loader_wait;

// Boot loader to kickstart system on a reset
// Required because the ROM is overwritten and needs to be reloaded every reset
// First detect end of reset pulse to kickstart download
logic reset_ne;
logic first_byte;
edge_det reset_edge_det(.clk_sys(clk_sys), .signal(reset), .neg_edge(reset_ne));

logic [15:0] read_addr;
logic [7:0] read_data;
always @(posedge clk_sys)
begin
	if(reset_ne)
	begin
		read_addr <= 'b0;
		loader_addr <= 'b0;
		loader_wr <= 1'b0;
		execute_enable <= 1'b0;
		loader_download <= 1'b1;
		execute_addr <= 'b0;
	end
	else begin
		if(loader_download) 
		begin
			if(~loader_wr) 
			begin
				// Transfer loaded byte to loader
				loader_data <= read_data;
				loader_wr <= 1'b1;
			end
			else begin
				loader_wr <= 1'b0;
				loader_addr <= loader_addr + 'd1;
				read_addr <= read_addr + 'd1;
				if(read_addr >= BOOT_ROM_END)
				begin
					loader_download <= 1'b0;
					execute_enable <= 1'b1;
				end
			end
		end		
		if(execute_enable) execute_enable <= 1'b0;
	end
end

// Rom containing boot rom code to transfer to address 0
boot_loader boot_loader
(
	.address(read_addr),
	.data(read_data)
);



pcw_core pcw_core
(
	.reset(reset),
	.clk_sys(clk_sys),

	.joy0(joystick_0),
	.joy1(joystick_1),
	.joy_type(status[12:10]),
	.ps2_key(ps2_key),
	.ps2_mouse(ps2_mouse),
	.mouse_type(status[14:13]),

	.RGB(RGB),
	.hsync(HSync),
	.vsync(VSync),
	.hblank(HBlank),
	.vblank(VBlank),
	.ce_pix(ce_pix),

	.LED(LED),
	.audiomix(audiomix),

	.disp_color(status[6:5]),
	.ntsc(status[7]),
	.overclock(status[9:8]),
	.model(status[4]),
	.memory_size(status[16:15]),
	.dktronics(status[17]),
	.fake_colour_mode(status[19:18]),

	.dn_clk(clk_sys),
	.dn_go(loader_download),
	.dn_wr(loader_wr),
	.dn_addr(loader_addr),			// CPU = 0000-FFFF; cassette = 10000-1FFFF
	.dn_data(loader_data),

	.execute_addr(execute_addr),
	.execute_enable(execute_enable),

	.img_mounted(img_mounted),
	.img_readonly(img_readonly),
	.img_size(img_size),
	.density({1'b1, status[4]}),		// 8256/512 = A=SD, 9512+ A=DD

	.sd_lba(sd_lba),
	.sd_rd(sd_rd),
	.sd_wr(sd_wr),
	.sd_ack(sd_ack),
	.sd_buff_addr(sd_buff_addr),
	.sd_buff_dout(sd_buff_dout),
	.sd_buff_din(sd_buff_din),
	.sd_dout_strobe(sd_buff_wr),
	// SD RAM signals not explicitly named
	.locked(locked),
	.*	
);
///////////////////////////////////////////////////
wire        ce_pix;
wire [23:0] RGB;
wire        HSync,VSync,HBlank,VBlank;

wire  [2:0] scale = status[3:1];

wire ypbpr;


video_mixer #(.LINE_LENGTH(1024)) video_mixer
(
	.*,

   .ce_pix_actual(ce_pix),
	.scanlines(0),
	.scandoubler_disable(scale || forced_scandoubler),
	.hq2x(scale==3'b001),
   .ypbpr_full(0),
   .line_start(0),
   .mono(0),
	.B(RGB[7:2]),
	.G(RGB[15:10]),
	.R(RGB[23:18])
);

wire  [8:0] audiomix;
dac #(
   .c_bits                              (9))
audiodac_l(
   .clk_i                               (clk_sys ),
   .res_n_i                             (1       ),
   .dac_i                               (audiomix),
   .dac_o                               (AUDIO_L )
  );

assign DAC_L={audiomix,1'b0};
assign DAC_R=DAC_L;
assign AUDIO_R=AUDIO_L;


endmodule
