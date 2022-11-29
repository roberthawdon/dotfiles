-- This file can be loaded by calling `lua require('plugins')` from your init.vim

return require('packer').startup(function()
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  use 'Th3Whit3Wolf/one-nvim'

  use 'feline-nvim/feline.nvim'

  use 'Iron-E/nvim-highlite'

  use 'liuchengxu/vista.vim'

  use {
    'romgrk/barbar.nvim',
    requires = {'kyazdani42/nvim-web-devicons'}
  }

  use 'lewis6991/gitsigns.nvim'

  use 'scrooloose/syntastic'
  use 'rodjek/vim-puppet'
  use 'godlygeek/tabular'
  use 'msanders/snipmate.vim'
  use 'tpope/vim-fugitive'
  use 'junegunn/vim-easy-align'
  use 'elzr/vim-json'
  use 'pprovost/vim-ps1'

end)
