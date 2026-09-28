-- Fills in the Shinylive code block on the dashboard page (data/dashboard.qmd).
--
-- The code block in dashboard.qmd only names the app and the datasets:
--
--   ## embed-app: dashboard/app.R
--   ## embed-data: TCS_NOR.RDS Ministers_NOR.RDS ...
--
-- When Quarto renders the dashboard page, this filter replaces those lines with
-- the app code and the datasets (base64-encoded), which is the format the
-- shinylive filter expects. Paths are relative to dashboard.qmd. The filter only
-- runs for the dashboard page, so the rest of the website renders as normal, and
-- the page always uses the current app.R and data files.

local function read_file(path)
  local f = io.open(path, "rb")
  if not f then
    error("embed-app.lua: could not read " .. path)
  end
  local content = f:read("a")
  f:close()
  return content
end

-- Standard base64 (used only if Quarto's own encoder is unavailable)
local b64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
local function base64_fallback(data)
  local out = {}
  for i = 1, #data, 3 do
    local a, b, c = data:byte(i, i + 2)
    local n = a * 65536 + (b or 0) * 256 + (c or 0)
    local c1 = (n >> 18) & 63
    local c2 = (n >> 12) & 63
    local c3 = (n >> 6) & 63
    local c4 = n & 63
    out[#out + 1] = b64chars:sub(c1 + 1, c1 + 1) .. b64chars:sub(c2 + 1, c2 + 1) ..
      (b and b64chars:sub(c3 + 1, c3 + 1) or "=") .. (c and b64chars:sub(c4 + 1, c4 + 1) or "=")
  end
  return table.concat(out)
end

local function base64(data)
  if quarto and quarto.base64 and quarto.base64.encode then
    return quarto.base64.encode(data)
  end
  return base64_fallback(data)
end

local function input_dir()
  local input = (quarto and quarto.doc and quarto.doc.input_file) or PANDOC_STATE.input_files[1]
  return pandoc.path.directory(input)
end

function CodeBlock(el)
  -- Quarto keeps the braces of ```{shinylive-r} in the class name
  if not (el.classes:includes("{shinylive-r}") or el.classes:includes("shinylive-r")) then
    return nil
  end

  local lines, app, datasets = {}, nil, {}
  for line in (el.text .. "\n"):gmatch("(.-)\r?\n") do
    local a = line:match("^## embed%-app:%s*(.-)%s*$")
    local d = line:match("^## embed%-data:%s*(.-)%s*$")
    if a then
      app = a
    elseif d then
      for f in d:gmatch("%S+") do
        datasets[#datasets + 1] = f
      end
    else
      lines[#lines + 1] = line
    end
  end
  if not app then
    return nil
  end

  local dir = input_dir()
  local code = read_file(pandoc.path.join({ dir, app }))
  code = code:gsub("\r\n", "\n")
  code = code:gsub("\n+$", "")

  lines[#lines + 1] = "## file: app.R"
  lines[#lines + 1] = code
  for _, f in ipairs(datasets) do
    lines[#lines + 1] = "## file: " .. pandoc.path.filename(f)
    lines[#lines + 1] = "## type: binary"
    lines[#lines + 1] = base64(read_file(pandoc.path.join({ dir, f })))
  end

  el.text = table.concat(lines, "\n")
  return el
end
