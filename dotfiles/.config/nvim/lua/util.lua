local M = {}

function M.goto_prev_diagnostic()
  vim.diagnostic.jump({ count = -1 })
end

function M.goto_next_diagnostic()
  vim.diagnostic.jump({ count = 1 })
end

return M
