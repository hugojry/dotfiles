" Modus Operandi inspired colorscheme for Vim
" Follows minimal highlighting philosophy with focus on readability

highlight clear
if exists('syntax_on')
  syntax reset
endif

set background=light
let g:colors_name = 'modus-operandi'

" Enable true colors
if has('termguicolors')
  set termguicolors
endif

" Color Definitions
let s:none = 'NONE'

" Base values
let s:bg_main = '#ffffff'
let s:bg_dim = '#f2f2f2'
let s:bg_alt = '#f0f0f0'
let s:fg_main = '#000000'
let s:fg_dim = '#595959'
let s:fg_alt = '#193668'
let s:border = '#9f9f9f'
let s:border_highlight = '#3b3b3b'

" Common foreground values
let s:red = '#a60000'
let s:red_warmer = '#972500'
let s:red_cooler = '#a0132f'
let s:red_faint = '#7f0000'
let s:green = '#006800'
let s:green_warmer = '#316500'
let s:green_cooler = '#00663f'
let s:green_faint = '#2a5045'
let s:yellow = '#6f5500'
let s:yellow_warmer = '#884900'
let s:yellow_cooler = '#7a4f2f'
let s:yellow_faint = '#624416'
let s:blue = '#0031a9'
let s:blue_warmer = '#3548cf'
let s:blue_cooler = '#0000b0'
let s:blue_faint = '#003497'
let s:magenta = '#721045'
let s:magenta_warmer = '#8f0075'
let s:magenta_cooler = '#531ab6'
let s:magenta_faint = '#7c318f'
let s:cyan = '#005e8b'
let s:cyan_warmer = '#3f578f'
let s:cyan_cooler = '#005f5f'
let s:cyan_faint = '#005077'
let s:rust = '#8a290f'
let s:gold = '#80601f'
let s:olive = '#56692d'
let s:slate = '#2f3f83'
let s:indigo = '#4a3a8a'
let s:maroon = '#731c52'
let s:pink = '#7b435c'

" These foreground values can only be used for non-text elements with a 3:1
" contrast ratio. Combine with bg_main, bg_dim, bg_alt
let s:red_intense = '#d00000'
let s:green_intense = '#008900'
let s:yellow_intense = '#808000'
let s:blue_intense = '#0000ff'
let s:magenta_intense = '#dd22dd'
let s:cyan_intense = '#008899'

" Intense should only be combined with fg_main for text
let s:bg_red_intense = '#ff8f88'
let s:bg_green_intense = '#8adf80'
let s:bg_yellow_intense = '#f3d000'
let s:bg_blue_intense = '#bfc9ff'
let s:bg_magenta_intense = '#dfa0f0'
let s:bg_cyan_intense = '#a4d5f9'

" Subtle should be combined with fg_alt, fg_main
let s:bg_red_subtle = '#ffcfbf'
let s:bg_green_subtle = '#b3fabf'
let s:bg_yellow_subtle = '#fff576'
let s:bg_blue_subtle = '#ccdfff'
let s:bg_magenta_subtle = '#ffddff'
let s:bg_cyan_subtle = '#bfefff'

" Nuanced can be combined with their foreground ie. bg_red_nuanced with red
let s:bg_red_nuanced = '#fff1f0'
let s:bg_green_nuanced = '#ecf7ed'
let s:bg_yellow_nuanced = '#fff3da'
let s:bg_blue_nuanced = '#f3f3ff'
let s:bg_magenta_nuanced = '#fdf0ff'
let s:bg_cyan_nuanced = '#ebf6fa'

" Special purpose
let s:bg_completion = '#c0deff'
let s:bg_hl_line = '#d0d6ec'
let s:bg_paren_match = '#5fcfff'
let s:bg_paren_expression = '#efd3f5'
let s:bg_char_0 = '#7feaff'
let s:bg_char_1 = '#ffaaff'
let s:bg_char_2 = '#dff000'

" Common active/inactive colors
let s:bg_active = '#e0e0e0'
let s:fg_active = '#0a0a0a'
let s:bg_inactive = '#e9e9e9'
let s:fg_inactive = '#404148'

" Status line specific colors
let s:bg_status_line_active = '#c8c8c8'
let s:fg_status_line_active = '#0a0a0a'
let s:bg_status_line_inactive = '#e6e6e6'
let s:fg_status_line_inactive = '#585858'

" Tab bar colors for tab pages
let s:bg_tab_bar = '#dfdfdf'
let s:bg_tab_current = '#ffffff'
let s:bg_tab_other = '#c2c2c2'
let s:fg_tab_other = '#333333'
let s:bg_tab_alternate = '#c2c2e8'

" Git diffs
let s:bg_added = '#c1f2d1'
let s:bg_added_faint = '#d8f8e1'
let s:bg_added_refine = '#aee5be'
let s:bg_added_fringe = '#6cc06c'
let s:fg_added = '#005000'
let s:fg_added_intense = '#006700'
let s:bg_changed = '#ffdfa9'
let s:bg_changed_faint = '#ffefbf'
let s:bg_changed_refine = '#fac090'
let s:bg_changed_fringe = '#d7c20a'
let s:fg_changed = '#553d00'
let s:fg_changed_intense = '#655000'
let s:bg_removed = '#ffd8d5'
let s:bg_removed_faint = '#ffe9e9'
let s:bg_removed_refine = '#f3b5af'
let s:bg_removed_fringe = '#d84a4f'
let s:fg_removed = '#8f1313'
let s:fg_removed_intense = '#aa2222'
let s:bg_diff_context = '#f3f3f3'
let s:bg_visual = '#bdbdbd'

" Tinted variant
let s:tinted_bg_main = '#fbf7f0'
let s:tinted_bg_dim = '#efe9dd'
let s:tinted_border = '#9f9690'
let s:tinted_border_highlight = '#5c3f3d'
let s:tinted_bg_active = '#c9b9b0'
let s:tinted_bg_inactive = '#dfd5cf'
let s:tinted_red_faint = '#7f0000'
let s:tinted_bg_red_nuanced = '#ffe8f0'
let s:tinted_bg_green_nuanced = '#e0f5e0'
let s:tinted_bg_yellow_nuanced = '#f9ead0'
let s:tinted_bg_blue_nuanced = '#ebebff'
let s:tinted_bg_magenta_nuanced = '#f6e7ff'
let s:tinted_bg_cyan_nuanced = '#e1f3fc'
let s:tinted_bg_completion = '#f0c1cf'
let s:tinted_bg_hl_line = '#f1d5d0'
let s:tinted_bg_paren_match = '#7fdfcf'
let s:tinted_bg_status_line_active = '#cab9b2'
let s:tinted_bg_status_line_inactive = '#dfd9cf'
let s:tinted_bg_tab_bar = '#e0d4ce'
let s:tinted_bg_tab_current = '#fbf7f0'
let s:tinted_bg_tab_other = '#c8b8b2'
let s:tinted_bg_tab_alternate = '#c8b8ca'
let s:tinted_bg_added = '#c3ebc1'
let s:tinted_bg_added_faint = '#dcf8d1'
let s:tinted_bg_added_refine = '#acd6a5'
let s:tinted_bg_added_fringe = '#6cc06c'
let s:tinted_bg_changed_fringe = '#c0b200'
let s:tinted_bg_removed = '#f4d0cf'
let s:tinted_bg_removed_faint = '#ffe9e5'
let s:tinted_bg_removed_refine = '#f3b5a7'
let s:tinted_bg_removed_fringe = '#d84a4f'
let s:tinted_bg_diff_context = '#efe9df'

" Deuteranopia Variant
let s:deuteranopia_yellow = '#695500'
let s:deuteranopia_yellow_warmer = '#973300'
let s:deuteranopia_yellow_cooler = '#77492f'
let s:deuteranopia_bg_status_line_active = '#d0d6ff'
let s:deuteranopia_fg_status_line_active = '#0f0f0f'
let s:deuteranopia_bg_added = '#d5d7ff'
let s:deuteranopia_bg_added_faint = '#e6e6ff'
let s:deuteranopia_bg_added_refine = '#babcef'
let s:deuteranopia_bg_added_fringe = '#275acc'
let s:deuteranopia_fg_added = '#303099'
let s:deuteranopia_fg_added_intense = '#0303cc'
let s:deuteranopia_bg_changed = '#eecfdf'
let s:deuteranopia_bg_changed_faint = '#f0dde5'
let s:deuteranopia_bg_changed_refine = '#e0b0d0'
let s:deuteranopia_bg_changed_fringe = '#9f6ab0'
let s:deuteranopia_fg_changed = '#6f1343'
let s:deuteranopia_fg_changed_intense = '#7f0f9f'
let s:deuteranopia_bg_removed = '#f4f099'
let s:deuteranopia_bg_removed_faint = '#f6f6b7'
let s:deuteranopia_bg_removed_refine = '#ede06f'
let s:deuteranopia_bg_removed_fringe = '#c0b200'
let s:deuteranopia_fg_removed = '#553d00'
let s:deuteranopia_fg_removed_intense = '#7f6f00'

" Tritanopia Variant
let s:tritanopia_red_warmer = '#b21100'
let s:tritanopia_red_cooler = '#a0132f'
let s:tritanopia_red_faint = '#702000'
let s:tritanopia_yellow = '#695500'
let s:tritanopia_yellow_warmer = '#973300'
let s:tritanopia_yellow_cooler = '#77492f'
let s:tritanopia_cyan_warmer = '#3f578f'
let s:tritanopia_cyan_faint = '#004f5f'
let s:tritanopia_magenta_intense = '#cd22bd'
let s:tritanopia_bg_completion = '#afdfef'
let s:tritanopia_bg_hl_line = '#dfeaec'
let s:tritanopia_bg_char_0 = '#ff908f'
let s:tritanopia_bg_char_1 = '#bfbfff'
let s:tritanopia_bg_char_2 = '#5fcfdf'
let s:tritanopia_bg_status_line_active = '#afe0f2'
let s:tritanopia_fg_status_line_active = '#0f0f0f'
let s:tritanopia_bg_added = '#b5e7ff'
let s:tritanopia_bg_added_faint = '#c6f6ff'
let s:tritanopia_bg_added_refine = '#9adcef'
let s:tritanopia_bg_added_fringe = '#1782cc'
let s:tritanopia_fg_added = '#005079'
let s:tritanopia_fg_added_intense = '#0043aa'
let s:tritanopia_bg_changed = '#eecfdf'
let s:tritanopia_bg_changed_faint = '#f0dde5'
let s:tritanopia_bg_changed_refine = '#e0b0d0'
let s:tritanopia_bg_changed_fringe = '#9f6ab0'
let s:tritanopia_fg_changed = '#6f1343'
let s:tritanopia_fg_changed_intense = '#7f0f9f'

" Highlight Function
function! s:hi(group, fg, bg, attr)
  let l:attr = a:attr
  if empty(l:attr)
    let l:attr = 'none'
  endif
  
  let l:hi = ['hi', a:group]
  if !empty(a:fg)
    call add(l:hi, 'guifg='.a:fg)
  endif
  if !empty(a:bg)
    call add(l:hi, 'guibg='.a:bg)
  endif
  call add(l:hi, 'gui='.l:attr)
  
  execute join(l:hi, ' ')
endfunction

" Editor
call s:hi('Normal', s:fg_main, s:bg_main, '')
call s:hi('LineNr', s:fg_dim, s:none, '')
call s:hi('CursorLine', '', s:bg_hl_line, '')
call s:hi('CursorLineNr', s:fg_dim, '', '')
call s:hi('Search', s:fg_main, s:bg_cyan_intense, '')
call s:hi('IncSearch', s:fg_main, s:bg_cyan_intense, '')
call s:hi('CurSearch', s:fg_main, s:bg_yellow_intense, '')
call s:hi('Visual', s:fg_main, s:bg_visual, '')

" Syntax
call s:hi('Comment', s:fg_dim, '', '')
call s:hi('Constant', s:blue_cooler, '', '')
call s:hi('String', s:blue_warmer, '', '')
call s:hi('Character', s:blue_warmer, '', '')
call s:hi('Number', s:fg_main, '', '')
call s:hi('Boolean', s:fg_main, '', '')
call s:hi('Float', s:fg_main, '', '')

call s:hi('Identifier', s:fg_main, '', '')
call s:hi('Function', s:magenta, '', '')

call s:hi('Statement', s:fg_main, '', '')
call s:hi('Conditional', s:fg_main, '', '')
call s:hi('Repeat', s:fg_main, '', '')
call s:hi('Label', s:fg_main, '', '')
call s:hi('Operator', s:fg_main, '', '')
call s:hi('Keyword', s:magenta_cooler, '', '')
call s:hi('Exception', s:red, '', '')

call s:hi('PreProc', s:red_cooler, '', '')
call s:hi('Include', s:fg_main, '', '')
call s:hi('Define', s:fg_main, '', '')
call s:hi('Macro', s:fg_main, '', '')
call s:hi('PreCondit', s:fg_main, '', '')

call s:hi('Type', s:fg_main, '', '')
call s:hi('StorageClass', s:fg_main, '', '')
call s:hi('Structure', s:fg_main, '', '')
call s:hi('Typedef', s:fg_main, '', '')

call s:hi('Special', s:cyan, '', '')
call s:hi('SpecialChar', s:cyan, '', '')
call s:hi('Tag', s:blue, '', '')
call s:hi('Delimiter', s:fg_main, '', '')
call s:hi('SpecialComment', s:fg_dim, '', 'italic')
call s:hi('Debug', s:red, '', '')

" Messages
call s:hi('Error', s:red, '', '')
call s:hi('Warning', s:yellow, '', '')
call s:hi('Info', s:blue, '', '')
call s:hi('Hint', s:green, '', '')

" UI Elements
call s:hi('StatusLine', s:fg_status_line_active, s:bg_status_line_active, '')
call s:hi('StatusLineNC', s:fg_status_line_inactive, s:bg_status_line_inactive, '')
call s:hi('VertSplit', s:border, '', '')
call s:hi('TabLine', s:fg_dim, s:bg_dim, '')
call s:hi('TabLineFill', '', s:bg_dim, '')
call s:hi('TabLineSel', s:fg_main, s:bg_main, '')

" Git
call s:hi('DiffAdd', s:green, '', '')
call s:hi('DiffChange', s:yellow, '', '')
call s:hi('DiffDelete', s:red, '', '')
call s:hi('DiffText', s:blue, '', '')

" Cleanup
delfunction s:hi
