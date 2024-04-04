return {
  {
    "nvim-orgmode/orgmode",
    dependencies = {
      {
        "nvim-treesitter/nvim-treesitter",
        lazy = true,
        config = function() end,
      },
    },
    event = { "LazyFile", "VeryLazy" },
    config = function()
      -- Load treesitter grammar for org
      require("orgmode").setup_ts_grammar()

      -- Setup orgmode
      require("orgmode").setup({
        org_agenda_files = "~/Org/agendas/*",
        org_default_notes_file = "~/org/tutorial.org",
      })

      require("nvim-treesitter.configs").setup({
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = { "org" },
        },
        ensure_installed = { "org" }, -- Or run :TSUpdate org
      })
    end,
  },
  {
    "hrsh7th/nvim-cmp",
    config = function()
      require("cmp").setup({
        sources = {
          { name = "orgmode" },
        },
      })
    end,
  },
}
