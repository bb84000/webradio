program webradio;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, indylaz, webradio1, settings1, Radios1, lazbbaboutupdate
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Tuner radio web';
  Application.Scaled:=True;

  Application.Initialize;
  Application.CreateForm(TFWebRadioMain, FWebRadioMain);
  Application.CreateForm(TFSettings, FSettings);
  Application.CreateForm(TFRadios, FRadios);
  Application.Run;
end.

