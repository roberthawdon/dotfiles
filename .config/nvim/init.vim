syntax on
set termguicolors
set number

set expandtab
set mouse=a
set tabpagemax=99

set noshowmode

set clipboard+=unnamedplus

lua require('config')
lua require('plugins')
lua require('gitsigns-config')
lua require('feline-config')

