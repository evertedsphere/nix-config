{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.neovim = {
    enable = true;
    extraConfig = ''
      set nocompatible

      filetype plugin indent on
      syntax on

      "https://vim.fandom.com/wiki/Example_vimrc
      set hidden
      set wildmenu
      set showcmd
      set hlsearch
      set nomodeline
      set ignorecase
      set smartcase
      set backspace=indent,eol,start
      set autoindent
      "set nostartofline
      set ruler
      set laststatus=2
      set confirm
      set visualbell
      set t_vb=
      set mouse=a
      set cmdheight=2
      set nonumber
      set notimeout ttimeout ttimeoutlen=200
      set pastetoggle=<F11>
      set shiftwidth=2
      set softtabstop=2
      set expandtab
      set clipboard=unnamedplus
      set undofile
      set nobackup
      set nowritebackup
      set updatetime=300
      set shortmess+=c
      set signcolumn=yes
      set dir=~/.swp
      set list
      set listchars=nbsp:¬,tab:→\ ,extends:»,precedes:«,trail:·,space:·

      packloadall

      set background=dark
      set termguicolors
      let ayucolor="dark"
      colorscheme ayu

      hi Normal     ctermbg=NONE guibg=NONE
      hi LineNr     ctermbg=NONE guibg=NONE
      hi SignColumn ctermbg=NONE guibg=NONE
    '';
    plugins = with pkgs.vimPlugins; [
      ayu-vim
      vim-commentary
      vim-surround
      vim-nix
      colorizer
    ];
  };
}
