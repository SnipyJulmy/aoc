local function parse(line)
	local regex = "^(%d+),(%d+)$"
	local x, y = string.match(line, regex)
	return { x = tonumber(x), y = tonumber(y) }
end

local function area(p1, p2)
	return (math.abs(p1.x - p2.x) + 1) * (math.abs(p1.y - p2.y) + 1)
end

local function isStrictlyBetween(value, a, b)
	local min = math.min(a, b)
	local max = math.max(a, b)
	return value > min and value < max
end

local function isRectangleContained(r, vertices)
	local n = #vertices
	local x_min = math.min(r.p1.x, r.p2.x)
	local x_max = math.max(r.p1.x, r.p2.x)
	local y_min = math.min(r.p1.y, r.p2.y)
	local y_max = math.max(r.p1.y, r.p2.y)

	for i = 1, n do
		local vi = vertices[i]
		local vj = vertices[(i % n) + 1]

		if vi.x == vj.x then
			local polygon_x = vi.x

			if polygon_x > x_min and polygon_x < x_max then
				if isStrictlyBetween(y_max, vi.y, vj.y) then
					return false
				end
				if isStrictlyBetween(y_min, vi.y, vj.y) then
					return false
				end
			end
		end

		if vi.y == vj.y then
			local polygon_y = vi.y

			if polygon_y > y_min and polygon_y < y_max then
				if isStrictlyBetween(x_max, vi.x, vj.x) then
					return false
				end
				if isStrictlyBetween(x_min, vi.x, vj.x) then
					return false
				end
			end
		end
	end
	return true
end

local function isPointContained(p, vertices)
	local n = #vertices
	local cross = 0

	for i = 1, n do
		local vi = vertices[i]
		local vj = vertices[(i % n) + 1]

		-- horizontal boundary check
		if vi.y == vj.y and vi.y == p.y then
			local min = math.min(vi.x, vj.x)
			local max = math.max(vi.x, vj.x)
			if p.x >= min and p.x <= max then
				return true
			end
		end

		-- vertical boundary check
		if vi.x == vj.x and vi.x == p.x then
			local min = math.min(vi.y, vj.y)
			local max = math.max(vi.y, vj.y)
			if p.y >= min and p.y <= max then
				return true
			end
		end

		-- ray casting
		local y_min = math.min(vi.y, vj.y)
		local y_max = math.max(vi.y, vj.y)
		if y_min <= p.y and y_max > p.y and vi.x == vj.x and vi.x > p.x then
			cross = cross + 1
		end
	end

	return (cross % 2) == 1
end

local function isRectangleFullyContained(rectangle, vertices)
	if not isPointContained(rectangle.p3, vertices) or not isPointContained(rectangle.p4, vertices) then
		return false
	end

	if not isRectangleContained(rectangle, vertices) then
		return false
	end

	return true
end

local function score1(points)
	local maxArea = 0
	for i = 1, #points - 1 do
		for j = i + 1, #points do
			local a = area(points[i], points[j])
			if a > maxArea then
				maxArea = a
			end
		end
	end
	return maxArea
end

local function score2(points)
	local maxArea = 0
	for i = 1, #points - 1 do
		for j = i + 1, #points do
			local p1 = points[i]
			local p2 = points[j]
			local a = area(p1, p2)
			if a > maxArea then
				local p3 = { x = p1.x, y = p2.y }
				local p4 = { x = p2.x, y = p1.y }
				local rectangle = { p1 = p1, p2 = p2, p3 = p3, p4 = p4 }
				if isRectangleFullyContained(rectangle, points) then
					maxArea = a
				end
			end
		end
	end
	return maxArea
end

local function solve(points)
	return score1(points), score2(points)
end

local filepath = arg[1]
local file = io.open(filepath, "r")
local points = {}
if file then
	for line in file:lines() do
		table.insert(points, parse(line))
	end
end

local part1, part2 = solve(points)
print("Part 1: " .. part1)
print("Part 2: " .. part2)
