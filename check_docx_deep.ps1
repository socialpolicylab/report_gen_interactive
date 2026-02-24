Add-Type -AssemblyName System.IO.Compression.FileSystem
$z = [System.IO.Compression.ZipFile]::OpenRead('D:\report_education\report_interactive_lite\docs\report_gen_word.docx')

# Check styles.xml for issues
$e = $z.GetEntry('word/styles.xml')
$s = $e.Open()
$r = New-Object System.IO.StreamReader($s)
$styles = $r.ReadToEnd()
$r.Close()

try {
    $xml = [xml]$styles
    Write-Host "styles.xml: VALID XML"
} catch {
    Write-Host "styles.xml: XML ERROR - $($_.Exception.Message)"
}

# Check numbering.xml
$e2 = $z.GetEntry('word/numbering.xml')
$s2 = $e2.Open()
$r2 = New-Object System.IO.StreamReader($s2)
$num = $r2.ReadToEnd()
$r2.Close()

try {
    $xml2 = [xml]$num
    Write-Host "numbering.xml: VALID XML"
} catch {
    Write-Host "numbering.xml: XML ERROR - $($_.Exception.Message)"
}

# Check footnotes.xml
$e3 = $z.GetEntry('word/footnotes.xml')
$s3 = $e3.Open()
$r3 = New-Object System.IO.StreamReader($s3)
$fn = $r3.ReadToEnd()
$r3.Close()

try {
    $xml3 = [xml]$fn
    Write-Host "footnotes.xml: VALID XML"
} catch {
    Write-Host "footnotes.xml: XML ERROR - $($_.Exception.Message)"
}

# Check comments.xml
$e4 = $z.GetEntry('word/comments.xml')
$s4 = $e4.Open()
$r4 = New-Object System.IO.StreamReader($s4)
$cm = $r4.ReadToEnd()
$r4.Close()

try {
    $xml4 = [xml]$cm
    Write-Host "comments.xml: VALID XML"
} catch {
    Write-Host "comments.xml: XML ERROR - $($_.Exception.Message)"
}

# Check settings.xml
$e5 = $z.GetEntry('word/settings.xml')
$s5 = $e5.Open()
$r5 = New-Object System.IO.StreamReader($s5)
$set = $r5.ReadToEnd()
$r5.Close()
Write-Host ""
Write-Host "=== settings.xml ==="
Write-Host $set

# Check for footnotes rels
$e6 = $z.GetEntry('word/_rels/footnotes.xml.rels')
$s6 = $e6.Open()
$r6 = New-Object System.IO.StreamReader($s6)
$fnrels = $r6.ReadToEnd()
$r6.Close()
Write-Host ""
Write-Host "=== footnotes.xml.rels ==="
Write-Host $fnrels

# Check document.xml for potential issues - look for unusual elements
$e7 = $z.GetEntry('word/document.xml')
$s7 = $e7.Open()
$r7 = New-Object System.IO.StreamReader($s7)
$doc = $r7.ReadToEnd()
$r7.Close()

# Check for cross-references or bookmarks
$bookmarks = [regex]::Matches($doc, 'w:bookmarkStart[^/]*w:name="([^"]*)"')
Write-Host ""
Write-Host "=== Bookmarks: $($bookmarks.Count) ==="
foreach ($b in $bookmarks) { Write-Host $b.Groups[1].Value }

# Check for any unusual/non-standard elements
$nsMatches = [regex]::Matches($doc, 'xmlns:(\w+)="([^"]*)"')
Write-Host ""
Write-Host "=== Namespaces ==="
foreach ($ns in $nsMatches) { Write-Host "$($ns.Groups[1].Value) = $($ns.Groups[2].Value)" }

$z.Dispose()
