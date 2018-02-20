
Dim xl

Dim fso
Dim dir
Dim File

Dim wb
Dim sh

Set xl = CreateObject("Excel.Application")
Set fso = CreateObject("Scripting.FileSystemObject")
dir = fso.GetAbsolutePathName("data/rig_counts")

With xl
  .Visible = False
  .DisplayAlerts = False
  .AskToUpdateLinks = False
  .AlertBeforeOverwriting = False
  .ScreenUpdating = False
End With

For Each f in fso.GetFolder(dir).Files
  If LCase(fso.GetExtensionName(f.Name)) = "xlsb" Then
    Set wb = xl.Workbooks.Open(f.Path, 0, True)
      For Each sh In wb.Worksheets
        sh.SaveAs dir & "\" & sh.Name & ".csv", 6
      Next
    wb.Close
  End If
Next

xl.Quit

Set wb = Nothing
Set xl = Nothing

WScript.Quit
