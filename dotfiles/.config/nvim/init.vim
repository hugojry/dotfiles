call plug#begin('~/.config/nvim/plugged')

Plug 'tpope/vim-fugitive'
Plug 'altercation/vim-colors-solarized'

call plug#end()

set background=light
colorscheme solarized

set expandtab
tnoremap <Esc> <C-\><C-n>
