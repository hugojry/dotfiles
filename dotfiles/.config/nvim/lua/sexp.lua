local M = {}
local a = vim.api
local ts = vim.treesitter

local sexp_node_types = {
  list_lit = true,
  vec_lit  = true,
  map_lit  = true,
  kwd_lit  = true,
  sym_lit  = true,
  num_lit  = true,
  str_lit  = true
}

local list_node_types = {
  list_lit = true,
  vec_lit  = true,
  map_lit  = true
}

local openers = {
  ["("] = true,
  ["["] = true,
  ["{"] = true
}

local closers = {
  [")"] = true,
  ["]"] = true,
  ["}"] = true
}

---@param node TSNode
---@return TSNode?
local function find_node_types(types, node)
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
local function pos_order(row_a, col_a, row_b, col_b)
  return (row_a == row_b and col_b - col_a) or row_b - row_a
end

---@param node TSNode
---@return TSNode[]?
local function is_between(node, row, col)
  local child_iter = node:iter_children()
  local prev

  for child in child_iter do
    local start_row, start_col, end_row, end_col = child:range()

    -- Then the cursor is between children of the current node
    if pos_order(row, col, start_row, start_col) > 0 then
      return { prev, child }
    -- Then the cursor falls inside one of the children of the current node
    elseif pos_order(row, col, end_row, end_col) > 0 then
      return
    end

    prev = child
  end
end

---@return integer, integer
local function get_cur()
  local row, col = unpack(a.nvim_win_get_cursor(0))
  row = row - 1
  return row, col
end

---@param row integer 0-indexed row
---@param col integer 0-indexed col
local function set_cur(row, col)
  a.nvim_win_set_cursor(0, { row + 1, col })
end

function M.adjacent_sexp(is_forward)
  local row, col = get_cur()

  local node = ts.get_node()
  if not node then return end

  -- Find a node that represents a type of sexp
  node = find_node_types(sexp_node_types, node)
  if not node then return end

  -- If the cursor is between two siblings of the current node then move
  -- to the start of the adjacent sibling
  local nodes = is_between(node, row, col)
  if nodes then
    local dest_row, dest_col = nodes[is_forward and 2 or 1]:range()
    a.nvim_win_set_cursor(0, { dest_row + 1, dest_col })
    return
  end

  if not is_forward then
    local node_start_row, node_start_col = node:range()
    if row ~= node_start_row or col ~= node_start_col then
      set_cur(node_start_row, node_start_col)
      return
    end
  end

  local dest_row, dest_col
  local f = is_forward and 'next_named_sibling' or 'prev_named_sibling'
  local sib = node[f](node)
  if not sib then
    if is_forward then
      _, _, dest_row, dest_col = node:range()
    else
      dest_row, dest_col = node:range()
    end
    if is_forward and a.nvim_get_mode().mode ~= 'no' then
      dest_col = dest_col - 1
    end
  else
    dest_row, dest_col = sib:range()
  end

  set_cur(dest_row, dest_col)
end

M.forward_sexp = function()
  for _ = 1, vim.v.count1 do M.adjacent_sexp(true) end
end

M.backward_sexp = function()
  for _ = 1, vim.v.count1 do M.adjacent_sexp(false) end
end

---@param start_row integer
---@param start_col integer
---@param end_row integer
---@param end_col integer
---@param dest_row integer
---@param dest_col integer
local function move_range(
  start_row, start_col, end_row, end_col, dest_row, dest_col
)
  local text = a.nvim_buf_get_text(0, start_row, start_col, end_row, end_col, {})
  -- dest starts after range starts
  if pos_order(start_row, start_col, dest_row, dest_col) > 0 then
    a.nvim_buf_set_text(0, dest_row, dest_col, dest_row, dest_col, text)
    if pos_order(end_row, end_col, dest_row, dest_col) > 0 then
      a.nvim_buf_set_text(0, start_row, start_col, end_row, end_col, {})
    else
      a.nvim_buf_set_text(0, start_row, start_col, dest_row, dest_col, {})
    end
  else
    a.nvim_buf_set_text(0, start_row, start_col, end_row, end_col, {})
    a.nvim_buf_set_text(0, dest_row, dest_col, dest_row, dest_col, text)
  end
end

---@param node TSNode
---@param count integer
local function slurp_left(node, count)
  local sib = node:prev_named_sibling()
  if not sib then return end
  for _ = 1, count - 1 do
    local sib_next = sib:prev_named_sibling()
    if not sib_next then
      break
    else
      sib = sib_next
    end
  end

  local row, col = node:range()
  local start_row, start_col = sib:range()
  move_range(row, col, row, col + 1, start_row, start_col)
  set_cur(start_row, start_col)
end

---@param node TSNode
---@param count integer
local function slurp_right(node, count)
  local sib = node:next_named_sibling()
  if not sib then return end
  for _ = 1, count - 1 do
    local sib_next = sib:next_named_sibling()
    if not sib_next then
      break
    else
      sib = sib_next
    end
  end

  local _, _, row, col = node:range()
  local _, _, end_row, end_col = sib:range()
  move_range(row, col - 1, row, col, end_row, end_col)
  set_cur(end_row, end_col - (row == end_row and 1 or 0))
end

---@param node TSNode
---@param count integer
local function barf_left(node, count)
  local named_child_count = node:named_child_count()
  if named_child_count < 2 then return end

  local retained_child = node:named_child(
    math.max(0, named_child_count - 1 - count)
  )
  if not retained_child then return end

  local _, _, row, col = node:range()
  local _, _, end_row, end_col = retained_child:range()
  move_range(row, col - 1, row, col, end_row, end_col)
  set_cur(end_row, end_col)
end

---@param node TSNode
---@param count integer
local function barf_right(node, count)
  local named_child_count = node:named_child_count()
  if named_child_count < 2 then return end

  local retained_child = node:named_child(
    math.min(count, named_child_count - 1)
  )
  if not retained_child then return end

  local row, col = node:range()
  local start_row, start_col = retained_child:range()
  move_range(row, col, row, col + 1, start_row, start_col)
  set_cur(start_row, start_col - (row == start_row and 1 or 0))
end

---@param is_left boolean
---@param count integer
local function slurp_barf(is_left, count)
  local node = ts.get_node()
  if not node then return end

  node = find_node_types(list_node_types, node)
  if not node then return end

  local row, col = get_cur()

  local cur_text = a.nvim_buf_get_text(0, row, col, row, col + 1, {})[1]
  if openers[cur_text] then
    (is_left and slurp_left or barf_right)(node, count)
  elseif closers[cur_text] then
    (is_left and barf_left or slurp_right)(node, count)
  else
    (is_left and slurp_left or slurp_right)(node, count)
  end
end

function M.slurp_barf_left_op()
  slurp_barf(true, vim.v.count1)
end

function M.slurp_barf_right_op()
  slurp_barf(false, vim.v.count1)
end

---@generic T
---@param is_branch fun(t: T): boolean
---@param children fun(t: T): fun(): T
---@param root T
---@return fun(): T
local function iter_tree(is_branch, children, root)
  return coroutine.wrap(function()
    local function dfs(n)
      coroutine.yield(n)

      if is_branch(n) then
        for c in children(n) do
          dfs(c)
        end
      end
    end

    dfs(root)
  end)
end

---@param node TSNode
---@param range integer[]
local function overlaps_range(node, range)
  local start_row, start_col, end_row, end_col = unpack(range)
  local node_start_row, node_start_col, node_end_row, node_end_col = node:range()
  return
    pos_order(end_row, end_col, node_start_row, node_start_col) < 0 and
    pos_order(start_row, start_col, node_end_row, node_end_col) > 0
end

---@param node TSNode
---@param range integer[]
local function range_contains_node(node, range)
  local start_row, start_col, end_row, end_col = unpack(range)
  local node_start_row, node_start_col, node_end_row, node_end_col = node:range()
  return pos_order(start_row, start_col, node_start_row, node_start_col) >= 0 and
    pos_order(end_row, end_col, node_end_row, node_end_col) <= 0
end

---@param root TSNode
---@param range integer[]
local function nodes_in_range(root, range)
  local function is_branch(node)
    return not range_contains_node(node, range)
  end

  local function children(node)
    return vim.iter(node:iter_children()):filter(function(n)
      return n:named() and overlaps_range(n, range)
    end)
  end

  return vim.iter(iter_tree(is_branch, children, root)):filter(function(n)
    return range_contains_node(n, range)
  end)
end

---@param range integer[]
---@param lines string[]
local function set_text(range, lines)
  local start_row, start_col, end_row, end_col = unpack(range)
  a.nvim_buf_set_text(0, start_row, start_col, end_row, end_col, lines)
end

---@param s string
---@param variables table
local function template(s, variables)
  for k, v in pairs(variables) do
    s = string.gsub(s, "${" .. k .. "}", tostring(v))
  end
  return s
end

---@param start_row integer
---@param end_row integer
local function cleanup_whitespace(start_row, end_row)
  local range_string = tostring(start_row + 1) .. "," .. tostring(end_row + 1)
  vim.cmd(template([[
    silent ${range} s/\v(^\s*)@<!\s//g
    silent ${range} g/^\s*$/d
  ]], { range = range_string } ))
end

---@param start_row integer
---@param end_row integer
local function linewise_delete(start_row, end_row)
  -- We're looking for the deepest node that contains the entire range.
  -- This nodes serves as the root for tree traversal.
  local root = ts.get_node()
  if not root then return end

  local range = { start_row, 0, end_row + 1, 0 }

  while not ts.node_contains(root, range) do
    root = root:parent()
    if not root then return end
  end

  for n in vim.iter(vim.iter(nodes_in_range(root, range)):totable()):rev() do
    set_text({ n:range() }, {})
  end

  cleanup_whitespace(start_row, end_row)
end

function M.delete_op(type)
  -- Deleting is inclusive
  if type == "line" then
    local start_row = a.nvim_buf_get_mark(0, "[")[1] - 1
    local end_row = a.nvim_buf_get_mark(0, "]")[1] - 1
    linewise_delete(math.min(start_row, end_row), math.max(start_row, end_row))
  elseif type == "char" then
    a.nvim_feedkeys('d`]x', 'n', false)
  end
end

function M.slurp_barf_left()
  vim.go.operatorfunc = "v:lua.require'sexp'.slurp_barf_left_op"
  return "g@l"
end

function M.slurp_barf_right()
  vim.go.operatorfunc = "v:lua.require'sexp'.slurp_barf_right_op"
  return "g@l"
end

function M.delete()
  vim.go.operatorfunc = "v:lua.require'sexp'.delete_op"
  return "g@"
end

return M
