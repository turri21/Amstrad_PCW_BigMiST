`default_nettype none

module guest_top(
	input         CLOCK_27,
`ifdef USE_CLOCK_50
	input         CLOCK_50,
`endif

	output        LED,
	output [VGA_BITS-1:0] VGA_R,
	output [VGA_BITS-1:0] VGA_G,
	output [VGA_BITS-1:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,

`ifdef USE_HDMI
	output        HDMI_RST,
	output  [7:0] HDMI_R,
	output  [7:0] HDMI_G,
	output  [7:0] HDMI_B,
	output        HDMI_HS,
	output        HDMI_VS,
	output        HDMI_PCLK,
	output        HDMI_DE,
	inout         HDMI_SDA,
	inout         HDMI_SCL,
	input         HDMI_INT,
`endif

	input         SPI_SCK,
	inout         SPI_DO,
	input         SPI_DI,
	input         SPI_SS2,    // data_io
	input         SPI_SS3,    // OSD
	input         CONF_DATA0, // SPI_SS for user_io

`ifdef USE_QSPI
	input         QSCK,
	input         QCSn,
	inout   [3:0] QDAT,
`endif
`ifndef NO_DIRECT_UPLOAD
	input         SPI_SS4,
`endif

	output [12:0] SDRAM_A,
	inout  [15:0] SDRAM_DQ,
	output        SDRAM_DQML,
	output        SDRAM_DQMH,
	output        SDRAM_nWE,
	output        SDRAM_nCAS,
	output        SDRAM_nRAS,
	output        SDRAM_nCS,
	output  [1:0] SDRAM_BA,
	output        SDRAM_CLK,
	output        SDRAM_CKE,

`ifdef DUAL_SDRAM
	output [12:0] SDRAM2_A,
	inout  [15:0] SDRAM2_DQ,
	output        SDRAM2_DQML,
	output        SDRAM2_DQMH,
	output        SDRAM2_nWE,
	output        SDRAM2_nCAS,
	output        SDRAM2_nRAS,
	output        SDRAM2_nCS,
	output  [1:0] SDRAM2_BA,
	output        SDRAM2_CLK,
	output        SDRAM2_CKE,
`endif

	output        AUDIO_L,
	output        AUDIO_R,
`ifdef I2S_AUDIO
	output        I2S_BCK,
	output        I2S_LRCK,
	output        I2S_DATA,
`endif
`ifdef SPDIF_AUDIO
	output        SPDIF,
`endif
`ifdef USE_AUDIO_IN
	input         AUDIO_IN,
`endif
	input         UART_RX,
	output        UART_TX

);

`ifdef NO_DIRECT_UPLOAD
localparam bit DIRECT_UPLOAD = 0;
wire SPI_SS4 = 1;
`else
localparam bit DIRECT_UPLOAD = 1;
`endif

`ifdef USE_QSPI
localparam bit QSPI = 1;
assign QDAT = 4'hZ;
`else
localparam bit QSPI = 0;
`endif

`ifdef VGA_8BIT
localparam VGA_BITS = 8;
`else
localparam VGA_BITS = 6;
`endif

`ifdef USE_HDMI
localparam bit HDMI = 1;
assign HDMI_RST = 1'b1;
`else
localparam bit HDMI = 0;
`endif

`ifdef BIG_OSD
localparam bit BIG_OSD = 1;
`define SEP "-;",
`else
localparam bit BIG_OSD = 0;
`define SEP
`endif

`ifdef USE_AUDIO_IN
wire TAPE_SOUND = AUDIO_IN;
`else
wire TAPE_SOUND = UART_RX;
`endif
	



localparam BOOT_ROM_END = 16'd275;	// Length of boot rom

`include "build_id.v"
localparam CONF_STR = {
	"PCW;;",
	"S0U,DSK,Mount A:;",
	"S1U,DSK,Mount B:;",
	`SEP
	"O4,Model,8256/8512,9256/9512+;",
	"OFG,Memory Size,256K,512K,1MB,2MB;",
	"O89,Clockspeed,4,8,16,32;",
	`SEP
	"O56,CRT Color,White,Green,Amber;",
	"OIJ,Fake Color,None,CGA,EGA;",
	"O7,Video System,PAL,NTSC;",
	"O13,Scandoubler Fx,None,HQ2x,CRT 25%,CRT 50%,CRT 75%;",
	`SEP
	"OAC,Joystick,None,Kempston,Spectravideo,Cascade,DKTronics;",
	"ODE,Mouse,None,AMX,Kempston,Keymouse;",
	"OH,DKTronics I/F,Disabled,Enabled;",
	`SEP
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

wire        no_csync;
wire        ypbpr;

wire [31:0] status;
wire  [1:0] buttons,switches;


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

wire        scandoubler_disable;

wire        key_pressed;
wire [7:0]  key_code;
wire        key_strobe;
wire        key_extended;

wire  [8:0] mouse_x;
wire  [8:0] mouse_y;
wire  [7:0] mouse_flags;
wire        mouse_strobe;

wire [24:0] ps2_mouse = { mouse_strobe_level, mouse_y[7:0], mouse_x[7:0], mouse_flags };
reg         mouse_strobe_level;
always @(posedge clk_sys) if (mouse_strobe) mouse_strobe_level <= ~mouse_strobe_level;

wire [10:0] ps2_key ={key_strobe,key_pressed,key_extended,key_code};
wire [15:0] joystick_0, joystick_1;

user_io #(.STRLEN($size(CONF_STR)>>3), .SD_IMAGES(2), .FEATURES(32'h0 | (BIG_OSD << 13) | (HDMI << 14))) user_io
(
	.clk_sys(clk_sys),
	.clk_sd(clk_sys),
	.conf_str(CONF_STR),

	.SPI_CLK(SPI_SCK),
	.SPI_SS_IO(CONF_DATA0),
	.SPI_MOSI(SPI_DI),
	.SPI_MISO(SPI_DO),

	.img_mounted(img_mounted),
	.img_size(img_size),
	.sd_conf(1'b0),
	.sd_ack_conf(),
	.sd_sdhc(1'b1),
	.sd_lba(sd_lba),
	.sd_rd(sd_rd),
	.sd_wr(sd_wr),
	.sd_ack(sd_ack),
	.sd_buff_addr(sd_buff_addr),
	.sd_din(sd_buff_din),
	.sd_dout(sd_buff_dout),
	.sd_dout_strobe(sd_buff_wr),

	.key_strobe(key_strobe),
	.key_code(key_code),
	.key_pressed(key_pressed),
	.key_extended(key_extended),

	.mouse_x(mouse_x),
	.mouse_y(mouse_y),
	.mouse_flags(mouse_flags),
	.mouse_strobe(mouse_strobe),

	.joystick_0(joystick_0),
	.joystick_1(joystick_1),

	.buttons(buttons),
	.status(status),
	.scandoubler_disable(scandoubler_disable),
	.ypbpr(ypbpr),
	.no_csync(no_csync)

);

mist_video #(.COLOR_DEPTH(8), .SD_HCNT_WIDTH(10), .USE_BLANKS(1'b1), .OUT_COLOR_DEPTH(VGA_BITS), .BIG_OSD(BIG_OSD)) mist_video (
	.clk_sys      (clk_sys     ),
	.SPI_SCK      (SPI_SCK    ),
	.SPI_SS3      (SPI_SS3    ),
	.SPI_DI       (SPI_DI     ),
	.R            (RGB[23:16]),
	.G            (RGB[15:8]),
	.B            (RGB[7:0]),
	.HSync(HSync),
	.VSync(VSync),
	.HBlank(HBlank),
	.VBlank(VBlank),
	.VGA_R        (VGA_R      ),
	.VGA_G        (VGA_G      ),
	.VGA_B        (VGA_B      ),
	.VGA_VS       (VGA_VS     ),
	.VGA_HS       (VGA_HS     ),
	.ce_divider   (1'b0       ),
	.scandoubler_disable(scandoubler_disable),
	.scanlines    (status[3:1]),
	.ypbpr        (ypbpr      ),
	.no_csync     (no_csync)

	);


wire reset = ~locked | status[0]|buttons[1];

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


wire tmpled;
assign LED=~tmpled;

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

	.LED(tmpled),
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


wire  [8:0] audiomix;
dac #(
   .c_bits                              (9))
audiodac_l(
   .clk_i                               (clk_sys ),
   .res_n_i                             (1       ),
   .dac_i                               (audiomix),
   .dac_o                               (AUDIO_L )
  );

assign AUDIO_R=AUDIO_L;

`ifdef I2S_AUDIO

wire [31:0] clk_rate =  32'd32_000_000;
wire [15:0] dac_out ={audiomix,7'b0000000};

//i2s i2s (
//	.reset(reset),
//	.clk(i2s_clock),
//	.clk_rate(clk_rate),
//
//	.sclk(I2S_BCK),
//	.lrclk(I2S_LRCK),
//	.sdata(I2S_DATA),
//
//	.left_chan (dac_out),
//	.right_chan(dac_out)
//);

audio_top audio_top
(
  .clk_50MHz  (CLOCK_27),
  .dac_MCLK   (),
  .dac_LRCK   (I2S_LRCK),
  .dac_SCLK   (I2S_BCK),
  .dac_SDIN   (I2S_DATA),
  .L_data     ({~dac_out[15],dac_out[14:0]}),
  .R_data     ({~dac_out[15],dac_out[14:0]})
);

`endif

endmodule
