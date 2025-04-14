local M = {}
local a = vim.api

---@param node TSNode
---@return TSNode?
function M.find_node_types(types, node)
  while not types[node:type()] do
    local parent = node:parent()
    if parent then
      node = parent
    else
      return
    end
  end
  return node
end

---@return integer
function M.pos_order(row_a, col_a, row_b, col_b)
  return (row_a == row_b and col_b - col_a) or row_b - row_a
end

---@return integer, integer
function M.get_cur()
  local row, col = unpack(a.nvim_win_get_cursor(0))
  row = row - 1
  return row, col
end

---@param row integer 0-indexed row
---@param col integer 0-indexed col
function M.set_cur(row, col)
  a.nvim_win_set_cursor(0, { row + 1, col })
end

return M
