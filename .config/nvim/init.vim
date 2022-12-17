syntax on
set termguicolors
set number

set expandtab
set mouse=a
set tabpagemax=99

set noshowmode

set clipboard+=unnamedplus

" Quickly insert an empty new line without entering insert mode
    nnoremap <Leader>o o<Esc>0"_D
    nnoremap <Leader>O O<Esc>0"_D

lua require('config')
lua require('plugins')
lua require('gitsigns-config')
lua require('feline-config')

