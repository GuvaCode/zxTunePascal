program ZxPlay;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CTypes, raylib, libZxTune, Contnrs, SyncObjs, Crt;

const
  DEFAULT_FREQ = 44100;
  DEFAULT_BITS = 16;
  DEFAULT_CHANNELS = 2;
  BUFFER_SIZE = 8192;

  POSITION_UPDATE_INTERVAL = 100; // ms
  PROGRESS_BAR_WIDTH = 30; // Ширина полоски прогресса

var
  ZxTunePlayer: ZXTuneHandle = nil;
  ZxTuneData: ZXTuneHandle = nil;
  ZxTuneModule: ZXTuneHandle = nil;
  ModuleInfo: ZXTuneModuleInfo;
  ShouldExit: Boolean = False;
  PositionLock: TCriticalSection;
  InfoShown: Boolean = False;

procedure LoadLibs();
var
  folder: PChar;
begin
  folder := '';
  {$IFDEF CPUX86_64}
     {$IFDEF LINUX}
       folder := 'dlls/lin64/';
     {$ENDIF}
     {$IFDEF WINDOWS}

       folder := 'dlls/win64/';
     {$ENDIF}
  {$ENDIF}
  {$IFDEF CPU386}
     {$IFDEF LINUX}
        folder := 'dlls/lin32/';
     {$ENDIF}
     {$IFDEF WINDOWS}
        folder := 'dlls/win32/';
     {$ENDIF}
  {$ENDIF}


  LoadZXTuneLibrary(folder + libZxTune.DEFAULT_LIB_NAME);
end;

procedure LoadModuleFile(const MusicFile: string);
var
  FileStream: TFileStream;
  FFileData: Pointer;
  FFileSize: NativeUInt;
begin
  try
    FileStream := TFileStream.Create(MusicFile, fmOpenRead or fmShareDenyWrite);
    try
      FFileSize := FileStream.Size;
      GetMem(FFileData, FFileSize);
      FileStream.ReadBuffer(FFileData^, FFileSize);
    finally
      FileStream.Free;
    end;

    ZxTuneData := ZXTune_CreateData(FFileData, FFileSize);
    if ZxTuneData = nil then
      raise Exception.Create('Failed to create ZXTune data');

    ZxTuneModule := ZXTune_OpenModule(ZxTuneData);
    if ZxTuneModule = nil then
      raise Exception.Create('Failed to open ZXTune module');

    if not ZXTune_GetModuleInfo(ZxTuneModule, ModuleInfo) then
      raise Exception.Create('Failed to get module info');

    ZxTunePlayer := ZXTune_CreatePlayer(ZxTuneModule);
    if ZxTunePlayer = nil then
      raise Exception.Create('Failed to create ZXTune player');


    ZXTune_SetPlayerLoopTrack(ZxTunePlayer, 1);

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      raise;
    end;
  end;
end;

procedure FormatTime(ms: Integer; out Minutes, Seconds: Integer);
begin
  Minutes := ms div 60000;
  Seconds := (ms div 1000) mod 60;
end;

procedure ShowPosition;
var
  PosMs, DurationMs: Integer;
  PosMin, PosSec: Integer;
  DurMin, DurSec: Integer;
  ProgressPos: Integer;
  ProgressBar: string;
  i: Integer;

begin
  // Выводим информацию о модуле только один раз
  if not InfoShown then
  begin
    ClrScr;
    WriteLn('ZX Tune Player. ', ZXTune_GetVersion);
    WriteLn;
    InfoShown := True;

    WriteLn('Positions: ' + IntToStr( ModuleInfo.Positions));
    WriteLn('Loop Position: ' + IntToStr( ModuleInfo.LoopPosition));
    WriteLn('Frames: ' + IntToStr( ModuleInfo.Frames));
    WriteLn('Loop Frame: ' + IntToStr(  ModuleInfo.LoopFrame));
    WriteLn('Channels: ' + IntToStr(  ModuleInfo.Channels));
    WriteLn('Initial Tempo: ' + IntToStr(  ModuleInfo.InitialTempo));




  end;

  PosMs := ZXTune_GetPositionMs(ZxTunePlayer, @ModuleInfo);
  DurationMs := ZXTune_GetDurationMs(ZxTunePlayer, @ModuleInfo);

  FormatTime(PosMs, PosMin, PosSec);
  FormatTime(DurationMs, DurMin, DurSec);

  // Рассчитываем позицию для полоски прогресса
  if DurationMs > 0 then
    ProgressPos := Round((PosMs / DurationMs) * PROGRESS_BAR_WIDTH)
  else
    ProgressPos := 0;

  // Строим полоску прогресса
  ProgressBar := '[';
  for i := 1 to PROGRESS_BAR_WIDTH do
  begin
    if i <= ProgressPos then
      ProgressBar := ProgressBar + '='
    else
      ProgressBar := ProgressBar + '-';
  end;
  ProgressBar := ProgressBar + ']';

  // Выводим только изменяемую часть (позицию и прогресс-бар)
  GotoXY(1, 12);  // Перемещаем курсор на строку после статической информации
  Write(Format('Position: %d:%.2d / %d:%.2d %s',
    [PosMin, PosSec, DurMin, DurSec, ProgressBar]), '      '); // Добавляем пробелы для очистки

  // Parameters::ZXTune::Sound::FRAMEDURATION


end;



procedure FillAudio(bufferData: Pointer; frames: LongWord); cdecl;
begin
   ZXTune_RenderSound(ZxTunePlayer, bufferData, frames);
end;

procedure PlayFile(Filename: string);
var
  Stream: TAudioStream;
  LastUpdate: QWord;  test:double;
begin
  InitAudioDevice();
  SetAudioStreamBufferSizeDefault(BUFFER_SIZE);

  Stream := LoadAudioStream(DEFAULT_FREQ, DEFAULT_BITS, DEFAULT_CHANNELS);
  if not IsAudioStreamValid(Stream) then
    raise Exception.Create('Failed to initialize audio stream');

  SetAudioStreamCallback(Stream, @FillAudio);
  PlayAudioStream(Stream);

  LoadModuleFile(Filename);

  LastUpdate := GetTickCount64;
  while not ShouldExit do
  begin
    if IsAudioStreamProcessed(Stream) then
      ResumeAudioStream(Stream);

    if GetTickCount64 - LastUpdate >= POSITION_UPDATE_INTERVAL then
    begin
     /// writeln(ModuleInfo.LoopPosition);

      ShowPosition;
      WriteLn('');

      writeln(ZXTune_GetCurrentPosition(ZxTunePlayer));

      writeln('Position Ms: '+ IntToStr(ZXTune_GetPositionMs(ZxTunePlayer, @ModuleInfo)));
      writeln('Total Ms:' + IntToStr (ZXTune_GetDurationMs(ZxTunePlayer, @ModuleInfo)));

      LastUpdate := GetTickCount64;

     if ZXTune_SetDoneSamples(ZxTunePlayer, @ModuleInfo) then
     Continue;

    end;

    if KeyPressed then
    begin
      ReadKey;
      ShouldExit := True;
    end;

    Sleep(1);
  end;

  StopAudioStream(Stream);
  CloseAudioDevice;

  ZXTune_CloseData(ZxTuneData);
  ZXTune_CloseModule(ZxTuneModule);
  ZXTune_DestroyPlayer(ZxTunePlayer);
  WriteLn;
end;

begin
  PositionLock := TCriticalSection.Create;
  ExitCode := 1;
  LoadLibs;

  if ParamCount = 1 then
  begin
    if SysUtils.FileExists(ParamStr(1)) then
    begin
      PlayFile(ParamStr(1));
      ClrScr;
      ExitCode := 0;
    end
    else
      WriteLn('File ', ParamStr(1), ' does not exist');
  end
  else
    WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' <supported file (pt2, pt3, etc.)>');
  PositionLock.Free;
end.



