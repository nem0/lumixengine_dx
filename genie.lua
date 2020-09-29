newoption {
	trigger = "dx12",
	description = "use dx12 backend"
}

project "renderer"
	files { 
		"src/**.c",
		"src/**.cpp",
		"src/**.h",
		"genie.lua"
	}
	excludes { "../../src/renderer/gpu/gpu.cpp" }

	if _OPTIONS["dx12"] then
		includedirs {"../../plugins/dx11/external/pix/Include/WinPixEventRuntime", "../../plugins/dx11/external/include/dx" }
		libdirs { "../../plugins/dx11/external/pix/bin/x64" }
		files { "../../plugins/dx11/external/pix/bin/x64/WinPixEventRuntime.dll" }
		configuration "**.dll"
			buildaction "Copy" -- todo - this does not work
		configuration {}
		excludes { "src/gpu_dx.cpp" }
	else
		excludes { "src/gpu_dx12.cpp" }
	end

if build_studio then
	project "studio"
		if _OPTIONS["dx12"] then
			libdirs { "../../plugins/dx11/external/pix/bin/x64" }
		end
		for conf,conf_dir in pairs({Debug="release", RelWithDebInfo="release"}) do
			for platform,target_platform in pairs({win="windows", linux="linux", }) do
				configuration { "x64", conf, target_platform }
				libdirs {"../dx11/external/lib/" .. platform .. "64" .. "_" .. binary_api_dir .. "/" .. conf_dir}
			end
			configuration {}
		end
end