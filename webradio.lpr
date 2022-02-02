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
  Forms, webradio1, settings1, Radios1, lazbbaboutupdate, equalizer1
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
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TFEqualizer, FEqualizer);
  Application.Run;
end.

