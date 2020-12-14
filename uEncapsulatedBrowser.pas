// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright � 2020 Salvador Diaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit uEncapsulatedBrowser;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.SyncObjs, System.SysUtils,
  {$ELSE}
  SyncObjs, SysUtils,
  {$ENDIF}
  uCEFTypes, uCEFBrowserThread, uCEFSnapshotParameters;

type
  TEncapsulatedBrowser = class
    protected
      FThread       : TCEFBrowserThread;
      FDelayMs      : integer;
      FScale        : single;
      FSnapshotPath : ustring;
      FErrorText    : ustring;
      FParameters   : TSnapshotParameters;

      procedure Thread_OnError(Sender: TObject);
      procedure Thread_OnSnapshotAvailable(Sender: TObject);

    public
      constructor Create(const aParameters : TSnapshotParameters);
      destructor  Destroy; override;
      procedure   LoadURL;

      property Parameters      : TSnapshotParameters read FParameters;
      property DelayMs         : integer    read FDelayMs        write FDelayMs;
      property Scale           : single     read FScale          write FScale;
      property SnapshotPath    : ustring    read FSnapshotPath   write FSnapshotPath;
      property ErrorText       : ustring    read FErrorText;
  end;

procedure CreateGlobalCEFApp(const parameters : TSnapshotParameters);
function  WaitForMainAppEvent : boolean;
procedure WriteResult;

implementation

uses
  uCEFApplication;

var
  MainAppEvent        : TEvent;
  EncapsulatedBrowser : TEncapsulatedBrowser = nil;
  vParameters : TSnapshotParameters;

procedure GlobalCEFApp_OnContextInitialized;
begin
  EncapsulatedBrowser := TEncapsulatedBrowser.Create(vParameters);
  EncapsulatedBrowser.LoadURL;
end;

function WaitForMainAppEvent : boolean;
begin
  Result := True;

  // Wait for 1 minute max.
  if (MainAppEvent.WaitFor(60000) = wrTimeout) then
    begin
      WriteLn('Timeout expired!');
      Result := False;
    end;
end;

procedure WriteResult;
begin
  if (EncapsulatedBrowser = nil) then
    WriteLn('There was a problem in the browser initialization')
   else
    if (length(EncapsulatedBrowser.ErrorText) > 0) then
      WriteLn(EncapsulatedBrowser.ErrorText)
     else
      WriteLn('Snapshot saved successfully as ' + EncapsulatedBrowser.SnapshotPath);
end;

procedure CreateGlobalCEFApp(const parameters : TSnapshotParameters);
begin
   GlobalCEFApp                            := TCefApplication.Create;
   GlobalCEFApp.WindowlessRenderingEnabled := True;
   GlobalCEFApp.EnableHighDPISupport       := True;
   GlobalCEFApp.ShowMessageDlg             := False;                    // This demo shouldn't show any window, just console messages.
   GlobalCEFApp.BlinkSettings              := 'hideScrollbars';         // This setting removes all scrollbars to capture a cleaner snapshot
   GlobalCEFApp.OnContextInitialized       := GlobalCEFApp_OnContextInitialized;

   var basePath := ExtractFilePath(ParamStr(0)) + 'Chromium87';
   SetCurrentDir(basePath);
   GlobalCEFApp.BrowserSubprocessPath      := 'cefHtmlSnapshot_sp.exe'; // This is the other EXE for the CEF subprocesses. It's on the same directory as this app.
   vParameters := parameters;

   GlobalCEFApp.StartMainProcess;
end;

constructor TEncapsulatedBrowser.Create(const aParameters : TSnapshotParameters);
begin
  inherited Create;

  FThread        := nil;
  FParameters    := aParameters;
  FDelayMs       := 500;
  FScale         := 1;    // This is the relative scale to a 96 DPI screen. It's calculated with the formula : scale = custom_DPI / 96
  FSnapshotPath  := 'snapshot.bmp';
  FErrorText     := '';
end;

destructor TEncapsulatedBrowser.Destroy;
begin
  if (FThread <> nil) then
    begin
      if FThread.TerminateBrowserThread then
        FThread.WaitFor;

      FreeAndNil(FThread);
    end;

  inherited Destroy;
end;

procedure TEncapsulatedBrowser.LoadURL;
begin
  if (FThread = nil) then
    begin
      FThread                     := TCEFBrowserThread.Create(Parameters);
      FThread.OnError             := Thread_OnError;
      FThread.OnSnapshotAvailable := Thread_OnSnapshotAvailable;
      FThread.Start;
    end
   else
    FThread.LoadUrl(Parameters.URL);
end;

procedure TEncapsulatedBrowser.Thread_OnError(Sender: TObject);
begin
  // This code is executed in the TCEFBrowserThread thread context while the main application thread is waiting for MainAppEvent.

  FErrorText := 'Error';

  if (FThread.ErrorCode <> 0) then
    FErrorText := FErrorText + ' ' + inttostr(FThread.ErrorCode);

  FErrorText := FErrorText + ' : ' + FThread.ErrorText;

  if (length(FThread.FailedUrl) > 0) then
    FErrorText := FErrorText + ' - ' + FThread.FailedUrl;

  MainAppEvent.SetEvent;
end;

procedure TEncapsulatedBrowser.Thread_OnSnapshotAvailable(Sender: TObject);
begin
  // This code is executed in the TCEFBrowserThread thread context while the main application thread is waiting for MainAppEvent.

  if not(FThread.SaveSnapshotToFile(FSnapshotPath)) then
    FErrorText := 'There was an error copying the snapshot';

  MainAppEvent.SetEvent;
end;

initialization
  MainAppEvent := TEvent.Create;

finalization
  MainAppEvent.Free;
  if (EncapsulatedBrowser <> nil) then FreeAndNil(EncapsulatedBrowser);

end.
