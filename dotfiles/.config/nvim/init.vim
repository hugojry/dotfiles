call plug#begin('~/.config/nvim/plugged')

Plug 'tpope/vim-dispatch'
Plug 'clojure-vim/vim-jack-in'
Plug 'tpope/vim-fireplace'
Plug 'tpope/vim-fugitive'
Plug 'guns/vim-sexp'

call plug#end()

set termguicolors
colorscheme tempus_totus

set hidden

set expandtab
tnoremap <Esc> <C-\><C-n>
