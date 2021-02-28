call plug#begin('~/.config/nvim/plugged')

Plug 'tpope/vim-fugitive'
Plug 'tjdevries/colorbuddy.nvim'
Plug 'ishan9299/modus-theme-vim', {'branch': 'stable'}

call plug#end()

lua require('colorbuddy').colorscheme('modus-operandi')

set expandtab
tnoremap <Esc> <C-\><C-n>
