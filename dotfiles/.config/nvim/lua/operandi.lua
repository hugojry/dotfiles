-- Modus Operandi inspired colorscheme for Neovim
-- Follows minimal highlighting philosophy with focus on readability

local M = {}

-- Color palette from Modus Operandi
local colors = {
  none = "NONE",

  -- Base values
  bg_main = "#ffffff",
  bg_dim = "#f2f2f2",
  bg_alt = "#f0f0f0",
  fg_main = "#000000",
  fg_dim = "#595959",
  fg_alt = "#193668",
  border = "#9f9f9f",
  border_highlight = "#3b3b3b",

  -- Common foreground values
  red = "#a60000",
  red_warmer = "#972500",
  red_cooler = "#a0132f",
  red_faint = "#7f0000",
  green = "#006800",
  green_warmer = "#316500",
  green_cooler = "#00663f",
  green_faint = "#2a5045",
  yellow = "#6f5500",
  yellow_warmer = "#884900",
  yellow_cooler = "#7a4f2f",
  yellow_faint = "#624416",
  blue = "#0031a9",
  blue_warmer = "#3548cf",
  blue_cooler = "#0000b0",
  blue_faint = "#003497",
  magenta = "#721045",
  magenta_warmer = "#8f0075",
  magenta_cooler = "#531ab6",
  magenta_faint = "#7c318f",
  cyan = "#005e8b",
  cyan_warmer = "#3f578f",
  cyan_cooler = "#005f5f",
  cyan_faint = "#005077",
  rust = "#8a290f",
  gold = "#80601f",
  olive = "#56692d",
  slate = "#2f3f83",
  indigo = "#4a3a8a",
  maroon = "#731c52",
  pink = "#7b435c",

  -- These foreground values can only be used for non-text elements with a 3:1
  -- contrast ratio. Combine with bg_main, bg_dim, bg_alt
  red_intense = "#d00000",
  green_intense = "#008900",
  yellow_intense = "#808000",
  blue_intense = "#0000ff",
  magenta_intense = "#dd22dd",
  cyan_intense = "#008899",

  -- Intense should only be combined with fg_main for text
  bg_red_intense = "#ff8f88",
  bg_green_intense = "#8adf80",
  bg_yellow_intense = "#f3d000",
  bg_blue_intense = "#bfc9ff",
  bg_magenta_intense = "#dfa0f0",
  bg_cyan_intense = "#a4d5f9",

  -- Subtle should be combined with fg_alt, fg_main
  bg_red_subtle = "#ffcfbf",
  bg_green_subtle = "#b3fabf",
  bg_yellow_subtle = "#fff576",
  bg_blue_subtle = "#ccdfff",
  bg_magenta_subtle = "#ffddff",
  bg_cyan_subtle = "#bfefff",

  -- Nuanced can be combined with their foreground ie. bg_red_nuanced with red
  bg_red_nuanced = "#fff1f0",
  bg_green_nuanced = "#ecf7ed",
  bg_yellow_nuanced = "#fff3da",
  bg_blue_nuanced = "#f3f3ff",
  bg_magenta_nuanced = "#fdf0ff",
  bg_cyan_nuanced = "#ebf6fa",

  -- Special purpose
  bg_completion = "#c0deff",
  bg_hl_line = "#d0d6ec",
  bg_paren_match = "#5fcfff",
  bg_paren_expression = "#efd3f5",
  bg_char_0 = "#7feaff",
  bg_char_1 = "#ffaaff",
  bg_char_2 = "#dff000",
  -- Common active/inactive colors
  bg_active = "#e0e0e0",
  fg_active = "#0a0a0a",
  bg_inactive = "#e9e9e9",
  fg_inactive = "#404148",
  -- Status line specific colors
  bg_status_line_active = "#c8c8c8",
  fg_status_line_active = "#0a0a0a",
  bg_status_line_inactive = "#e6e6e6",
  fg_status_line_inactive = "#585858",
  -- tab bar colors for tab pages
  bg_tab_bar = "#dfdfdf",
  bg_tab_current = "#ffffff",
  bg_tab_other = "#c2c2c2",
  fg_tab_other = "#333333",
  bg_tab_alternate = "#c2c2e8",
  -- git diffs
  bg_added = "#c1f2d1",
  bg_added_faint = "#d8f8e1",
  bg_added_refine = "#aee5be",
  bg_added_fringe = "#6cc06c",
  fg_added = "#005000",
  fg_added_intense = "#006700",
  bg_changed = "#ffdfa9",
  bg_changed_faint = "#ffefbf",
  bg_changed_refine = "#fac090",
  bg_changed_fringe = "#d7c20a",
  fg_changed = "#553d00",
  fg_changed_intense = "#655000",
  bg_removed = "#ffd8d5",
  bg_removed_faint = "#ffe9e9",
  bg_removed_refine = "#f3b5af",
  bg_removed_fringe = "#d84a4f",
  fg_removed = "#8f1313",
  fg_removed_intense = "#aa2222",
  bg_diff_context = "#f3f3f3",
  bg_visual = "#bdbdbd",

  -- Tinted variant
  tinted_bg_main = "#fbf7f0",
  tinted_bg_dim = "#efe9dd",
  tinted_border = "#9f9690",
  tinted_border_highlight = "#5c3f3d",
  tinted_bg_active = "#c9b9b0",
  tinted_bg_inactive = "#dfd5cf",
  tinted_red_faint = "#7f0000",
  tinted_bg_red_nuanced = "#ffe8f0",
  tinted_bg_green_nuanced = "#e0f5e0",
  tinted_bg_yellow_nuanced = "#f9ead0",
  tinted_bg_blue_nuanced = "#ebebff",
  tinted_bg_magenta_nuanced = "#f6e7ff",
  tinted_bg_cyan_nuanced = "#e1f3fc",
  tinted_bg_completion = "#f0c1cf",
  tinted_bg_hl_line = "#f1d5d0",
  tinted_bg_paren_match = "#7fdfcf",
  tinted_bg_status_line_active = "#cab9b2",
  tinted_bg_status_line_inactive = "#dfd9cf",
  tinted_bg_tab_bar = "#e0d4ce",
  tinted_bg_tab_current = "#fbf7f0",
  tinted_bg_tab_other = "#c8b8b2",
  tinted_bg_tab_alternate = "#c8b8ca",
  tinted_bg_added = "#c3ebc1",
  tinted_bg_added_faint = "#dcf8d1",
  tinted_bg_added_refine = "#acd6a5",
  tinted_bg_added_fringe = "#6cc06c",
  tinted_bg_changed_fringe = "#c0b200",
  tinted_bg_removed = "#f4d0cf",
  tinted_bg_removed_faint = "#ffe9e5",
  tinted_bg_removed_refine = "#f3b5a7",
  tinted_bg_removed_fringe = "#d84a4f",
  tinted_bg_diff_context = "#efe9df",

  -- Deuteranopia Variant
  deuteranopia_yellow = "#695500",
  deuteranopia_yellow_warmer = "#973300",
  deuteranopia_yellow_cooler = "#77492f",
  deuteranopia_bg_status_line_active = "#d0d6ff",
  deuteranopia_fg_status_line_active = "#0f0f0f",
  deuteranopia_bg_added = "#d5d7ff",
  deuteranopia_bg_added_faint = "#e6e6ff",
  deuteranopia_bg_added_refine = "#babcef",
  deuteranopia_bg_added_fringe = "#275acc",
  deuteranopia_fg_added = "#303099",
  deuteranopia_fg_added_intense = "#0303cc",
  deuteranopia_bg_changed = "#eecfdf",
  deuteranopia_bg_changed_faint = "#f0dde5",
  deuteranopia_bg_changed_refine = "#e0b0d0",
  deuteranopia_bg_changed_fringe = "#9f6ab0",
  deuteranopia_fg_changed = "#6f1343",
  deuteranopia_fg_changed_intense = "#7f0f9f",
  deuteranopia_bg_removed = "#f4f099",
  deuteranopia_bg_removed_faint = "#f6f6b7",
  deuteranopia_bg_removed_refine = "#ede06f",
  deuteranopia_bg_removed_fringe = "#c0b200",
  deuteranopia_fg_removed = "#553d00",
  deuteranopia_fg_removed_intense = "#7f6f00",

  -- Tritanopia Variant
  tritanopia_red_warmer = "#b21100",
  tritanopia_red_cooler = "#a0132f",
  tritanopia_red_faint = "#702000",
  tritanopia_yellow = "#695500",
  tritanopia_yellow_warmer = "#973300",
  tritanopia_yellow_cooler = "#77492f",
  tritanopia_cyan_warmer = "#3f578f",
  tritanopia_cyan_faint = "#004f5f",
  tritanopia_magenta_intense = "#cd22bd",
  tritanopia_bg_completion = "#afdfef",
  tritanopia_bg_hl_line = "#dfeaec",
  tritanopia_bg_char_0 = "#ff908f",
  tritanopia_bg_char_1 = "#bfbfff",
  tritanopia_bg_char_2 = "#5fcfdf",
  tritanopia_bg_status_line_active = "#afe0f2",
  tritanopia_fg_status_line_active = "#0f0f0f",
  tritanopia_bg_added = "#b5e7ff",
  tritanopia_bg_added_faint = "#c6f6ff",
  tritanopia_bg_added_refine = "#9adcef",
  tritanopia_bg_added_fringe = "#1782cc",
  tritanopia_fg_added = "#005079",
  tritanopia_fg_added_intense = "#0043aa",
  tritanopia_bg_changed = "#eecfdf",
  tritanopia_bg_changed_faint = "#f0dde5",
  tritanopia_bg_changed_refine = "#e0b0d0",
  tritanopia_bg_changed_fringe = "#9f6ab0",
  tritanopia_fg_changed = "#6f1343",
  tritanopia_fg_changed_intense = "#7f0f9f",
}

function M.setup()
  vim.cmd('hi clear')
  if vim.fn.exists('syntax_on') then
    vim.cmd('syntax reset')
  end
  vim.o.background = 'light'
  vim.o.termguicolors = true

  local groups = {
    -- Editor
    Normal = { fg = colors.fg_main, bg = colors.bg_main },
    LineNr = { fg = colors.light_gray },
    CursorLine = { bg = colors.very_light_gray },
    CursorLineNr = { fg = colors.gray },
    Search = { fg = colors.fg_main, bg = colors.bg_cyan_intense },
    IncSearch = { fg = colors.fg_main, bg = colors.bg_cyan_intense },
    CurSearch = { fg = colors.fg_main, bg = colors.bg_yellow_intense },
    Visual = { fg = colors.fg_main, bg = colors.bg_visual },

    -- Syntax
    Comment = { fg = colors.fg_dim },
    Constant = { fg = colors.blue_cooler },
    String = { fg = colors.blue_warmer },
    Character = { fg = colors.blue_warmer },
    Number = { fg = colors.fg_main },
    Boolean = { fg = colors.fg_main },
    Float = { fg = colors.fg_main },

    Identifier = { fg = colors.fg_main },
    Function = { fg = colors.magenta },

    Statement = { fg = colors.fg_main },
    Conditional = { fg = colors.fg_main },
    Repeat = { fg = colors.fg_main },
    Label = { fg = colors.fg_main },
    Operator = { fg = colors.fg_main },
    Keyword = { fg = colors.magenta_cooler },
    Exception = { fg = colors.red },

    PreProc = { fg = colors.red_cooler },
    Include = { fg = colors.fg_main },
    Define = { fg = colors.fg_main },
    Macro = { fg = colors.fg_main },
    PreCondit = { fg = colors.fg_main },

    Type = { fg = colors.fg_main },
    StorageClass = { fg = colors.fg_main },
    Structure = { fg = colors.fg_main },
    Typedef = { fg = colors.fg_main },

    Special = { fg = colors.cyan },
    SpecialChar = { fg = colors.cyan },
    Tag = { fg = colors.blue },
    Delimiter = { fg = colors.fg_main },
    SpecialComment = { fg = colors.gray, italic = true },
    Debug = { fg = colors.red },

    -- Treesitter
    ["@variable.builtin"] = { fg = colors.magenta_cooler },
    ["@constructor"] = { fg = colors.fg_main },
    ["@type.builtin"] = { fg = colors.magenta },
    ["@function.call"] = { fg = colors.fg_main },
    ["@function.method.call"] = { fg = colors.fg_main },

    -- Messages
    Error = { fg = colors.red },
    Warning = { fg = colors.yellow },
    Info = { fg = colors.blue },
    Hint = { fg = colors.green },

    -- UI
    StatusLine = {
      fg = colors.fg_status_line_active,
      bg = colors.bg_status_line_active,
    },
    StatusLineNC = {
      fg = colors.fg_status_line_inactive,
      bg = colors.bg_status_line_inactive
    },
    VertSplit = { fg = colors.light_gray },
    TabLine = { fg = colors.light_gray, bg = colors.fg_dim },
    TabLineFill = { bg = colors.bg_dim },
    TabLineSel = { fg = colors.fg_main, bg = colors.bg_dim },

    -- Git
    DiffAdd = { fg = colors.green },
    DiffChange = { fg = colors.yellow },
    DiffDelete = { fg = colors.red },
    DiffText = { fg = colors.blue },

    -- Telescope
    TelescopeBorder = { fg = colors.fg_dim },
    TelescopePromptBorder = { fg = colors.fg_dim },
    TelescopeResultsBorder = { fg = colors.fg_dim },
    TelescopePreviewBorder = { fg = colors.fg_dim },

    TelescopePromptPrefix = { fg = colors.blue },
    TelescopeSelection = { bg = colors.bg_visual },
    TelescopeSelectionCaret = { fg = colors.blue, bg = colors.bg_visual },

    TelescopePromptTitle = { fg = colors.blue },
    TelescopeResultsTitle = { fg = colors.blue },
    TelescopePreviewTitle = { fg = colors.blue },
  }

  -- Set all highlights
  for group, settings in pairs(groups) do
    vim.api.nvim_set_hl(0, group, settings)
  end
end

return M
