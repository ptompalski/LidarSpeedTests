param(
[string]$fname,
[string]$odir
# [string]$disk1,
# [string]$disk2

)

$workstationID = $env:computername

# $fname = [IO.Path]::GetFileNameWithoutExtension([System.IO.Path]::GetRandomFileName() )

# $Filepath = "H:/My Drive/_coding/benchmarkingALStools/system_monitoring/"+ $workstationID + $fname + ".txt"
$Filepath = $odir + "/"+ $workstationID + "-" + $fname + ".txt"

$totalRam = (Get-CimInstance Win32_PhysicalMemory | Measure-Object -Property capacity -Sum).Sum

while($true) {
    $date = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $cpuTime = (Get-Counter '\Processor(_Total)\% Processor Time').CounterSamples.CookedValue
    $availMem = (Get-Counter '\Memory\Available MBytes').CounterSamples.CookedValue
	
	# $disk_read = (Get-Counter '\logicaldisk('+ $disk1 + ')\% Disk Read Time').CounterSamples.CookedValue
	# $disk_read = (Get-Counter '\logicaldisk(*)\% Disk Read Time').CounterSamples.CookedValue
	# $disk_write = (Get-Counter '\logicaldisk('+ $disk2 + ')\% Disk Write Time').CounterSamples.CookedValue
	# $disk_write = (Get-Counter '\logicaldisk(*)\% Disk Write Time').CounterSamples.CookedValue
	# $HDD_G = (Get-Counter '\PhysicalDisk(4 G:)\% Disk Time').CounterSamples.CookedValue
	# $HDD_N = (Get-Counter '\PhysicalDisk(1 N:)\% Disk Time').CounterSamples.CookedValue
	
    # $date + ' > CPU: ' + $cpuTime.ToString("#,0.000") + '%, Avail. Mem.: ' + $availMem.ToString("N0") + 'MB (' + (104857600 * $availMem / $totalRam).ToString("#,0.0") + '%)' | Export-CSV -Append -NoTypeInformation $Filepath
    # $all = $date + ' > CPU: ' + $cpuTime.ToString("#,0.000") + '%, Avail. Mem.: ' + $availMem.ToString("N0") + 'MB (' + (104857600 * $availMem / $totalRam).ToString("#,0.0") + '%) ' + $disk_read.ToString("#,0.000")+ ' ' + $disk_write.ToString("#,0.000")
    
	# $all = $date + ' > CPU: ' + $cpuTime.ToString("#,0.000") + '%, Avail. Mem.: ' + $availMem.ToString("N0") + 'MB (' + (104857600 * $availMem / $totalRam).ToString("#,0.0") + '%) ' 
    
	$all = "$date,$cpuTime,$availMem"
	$all | Out-File -Append $Filepath
    $all 
	Start-Sleep -s 2
}


    # Format the data as CSV
    # $csvLine = "$timestamp,$cpuValue,$ramValue"

    # Append the data to the CSV file
    # $csvLine | Out-File -Append -FilePath $filePath