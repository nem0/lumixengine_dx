newoption {
	trigger = "dx12",
	description = "use dx12 backend"
}

newoption {
	trigger = "fsr2",
	description = "use fsr2"
}

newoption {
	trigger = "nodx",
	description = "do not use any dx backend"
}

local function setLibDirs()
	libdirs { "external/lib/win64_" .. binary_api_dir .. "/release"}
	libdirs { "external/pix/bin/x64" }
end

local use_fsr2 = false

if _OPTIONS["fsr2"] then
	if not _OPTIONS["dx12"] then
		printf("--fsr2 used, but not --dx12. FSR2 is only available on DX12");
	else
		use_fsr2 = true
	end
end


if _OPTIONS["nodx"] == nil then
	if use_fsr2 then
		project "fsr2"
			libType()
			files { 
				"src/fsr2.cpp",
				"src/fsr2.h",
			}
			if build_studio then
				files { "src/editor/fsr2_plugins.cpp" }
			end
			links { "engine" }
			includedirs { "external/include/fsr2" }
			useLua()
			defaultConfigurations()
			linkPlugin("fsr2")
	end

	project "renderer"
		files { 
			"src/**.c",
			"src/**.cpp",
			"src/**.h",
			"genie.lua"
		}
		removefiles { "src/fsr2.h", "src/fsr2.cpp", "src/editor/fsr2_plugins.cpp" }
		excludes { "../../src/renderer/gpu/gpu_gl.cpp" }

		if _OPTIONS["dx12"] then
			includedirs {"external/pix/Include/WinPixEventRuntime", "external/include/dx" }
			files { "external/pix/bin/x64/WinPixEventRuntime.dll" }
			copy { "external/pix/bin/x64/WinPixEventRuntime.dll" }
			excludes { "src/gpu_dx.cpp" }
			solution "LumixEngine"
				defines { "LUMIX_DX12" }
		else
			excludes { "src/gpu_dx12.cpp" }
			solution "LumixEngine"
				defines { "LUMIX_DX11" }
		end

		if _OPTIONS["dynamic-plugins"] then		
			libdirs { "external/lib/win64_" .. binary_api_dir .. "/release" }
			configuration {"vs*"}
				links { "dxguid" }
			configuration {}
		end

	if build_studio then
		project "studio"
			setLibDirs()
	end
	if build_app then
		project "app"
			setLibDirs()
	end
end