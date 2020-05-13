char font[] = "Inconsolata\\-dz:size=15";

const char *colorname[] = {
    /* 8 normal colors */
    "#3a3a3a",
	"#cc9393",
	"#7f9f7f",
	"#d0bf8f",
	"#6ca0a3",
	"#dc8cc3",
	"#93e0e3",
	"#dcdccc",

	/* 8 bright colors */
	"#7f9f7f",
	"#d78787",
	"#bfebbf",
	"#f0dfaf",
	"#8cd0d3",
	"#dc8cc3",
	"#93e0e3",
	"#ffffff",

	[255] = 0,

	/* more colors can be added after 255 to use with DefaultXX */
	"#dcdccc",
	"#dcdccc",
	"#3a3a3a",
};

/*
 * Default colors (colorname index)
 * foreground, background, cursor, reverse cursor
 */
unsigned int defaultfg = 188;
unsigned int defaultbg = 237;
unsigned int defaultcs = 188;
unsigned int defaultrcs = 257;

/*
 * Default shape of cursor
 * 2: Block ("█")
 * 4: Underline ("_")
 * 6: Bar ("|")
 * 7: Snowman ("☃")
 */
unsigned int cursorshape = 2;
