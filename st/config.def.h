char font[] = "Inconsolata\\-dz:size=15";

const char *colorname[] = {
    // lake
    "#22333a",
    "#c27171",
    "#63a690",
    "#a6a663",
    "#6385a6",
    "#BF9C86",
    "#63a39e",
    "#a9b6c1",
    "#456472",
    "#c27171",
    "#6dc2a3",
    "#bfc271",
    "#719bc2",
    "#BF9C86",
    "#71c2af",
    "#eff1f5",

    [255] = 0
};

/*
 * Default colors (colorname index)
 * foreground, background, cursor, reverse cursor
 */
unsigned int defaultfg = 7;
unsigned int defaultbg = 0;
unsigned int defaultcs = 7;
unsigned int defaultrcs = 0;

/*
 * Default shape of cursor
 * 2: Block ("█")
 * 4: Underline ("_")
 * 6: Bar ("|")
 * 7: Snowman ("☃")
 */
unsigned int cursorshape = 2;
