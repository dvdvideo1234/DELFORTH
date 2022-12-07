unit AppUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnit;

type tUrlStr = class(tObject)
       fDate : str5;
       fUrl  : string;
       constructor create(dat, url: string);
       destructor  destroy;
     end;

  procedure ReadList(inf: tStringList; name: string);
  procedure WriteList(outf: tStringList; name: string);
  procedure PackList(inl, outl: tStringList);

  var slr, slw: tStringList;

implementation


  const months : array[0..11] of string = (
         'Jan'  ,'Feb'  ,'Mar'  ,'Apr'  ,'May'  ,'Jun'
        ,'Jul'  ,'Aug'  ,'Sep'  ,'Oct'  ,'Nov'  ,'Dec'  );

       targAry: array[0..2] of string =(
        '('        ,'URL list from'        ,'Google Преводач');

  type tStrAry = array[0..10000] of string;
       PstrAry = ^tStrAry;

  var date: str5;

  function skipper(what: string; where: PstrAry; len: integer): integer;
  var l: integer;
  begin
    for result := 0 to pred(len) do begin
      l := length(where^[result]);
      if copy(what,1,L)  = where^[result] then exit;
    end;
    result :=  -1;
  end;

  constructor tUrlStr.create(dat, url: string);
  begin
    fdate := dat;
    furl  := url;
  end;

  destructor tUrlStr.destroy;
  begin
    furl  := '';
  end;

  procedure ReadList(inf: tStringList; name: string);
  begin
    inf.CaseSensitive:=true;
    inf.Clear;
    inf.Duplicates:=dupAccept;
    inf.LoadFromFile(name);
  end;

  procedure WriteList(outf: tStringList; name: string);
  var cnt: integer; s1, s2: string;  url: tUrlStr;
  begin
    cnt := 0;
    while cnt < outf.Count-2 do begin
      s1 := outf.Strings[cnt];
      s2 := outf.Strings[cnt+1];
      if s1 = s2 then outf.Delete(cnt) else inc(cnt);
    end;
    outf.Sorted:=false;   // !!!
    for cnt := 0 to outf.Count-1 do begin
      url := tUrlStr(outf.Objects[cnt]);
      s2 := url.fUrl;
      s1 := url.fDate;
      url.fUrl:='';
      outf.Strings[cnt] := outf.Strings[cnt]+'|'+s2+' '+s1;
    end;
    outf.SaveToFile(name);
  end;

  procedure PackList(inl, outl: tStringList);
  var targ, cnt, num: integer; s1, s2: string;
    s3: shortstring;
  begin
    cnt := 0;
    while cnt < inl.Count do begin
      if inl.Count - cnt >= 3 then begin
        repeat
          s1 := inl.Strings[cnt];
          inc(cnt);
        until s1 <> '';
        num := length(s1);
        targ := pos(' - YouTube',s1);
        if (targ + 10 > num) and (num > 10) then s1 := copy(s1,1,targ-1);
        s2 := inl.Strings[cnt];  inc(cnt);
        s3 := inl.Strings[cnt];  inc(cnt);
        if s3 <> '' then dec(cnt);
        targ := skipper(s1, @targAry, 3);
        case targ of
          2: continue;
          1: begin
               s3 := s1; cutStr(s3,','); s3 := trim(s3);
               s1 := cutStr(s3); s3 := trim(s3);
               targ := skipper(s1, @months, 12);   // months
               s2 := cutStr(s3); s3 := trim(s3);   // day
               num := strToNum(s2);                // year
               date:= copy(s3,3,2)+char(targ+ord('A'))+DigToChar(num);
               continue;
            end;
          0: begin
              targ := pos(')',s1);
              if targ <> 0 then delete(s1,1,targ);
            end;
        end;
        s1 := trim(s1);
        outl.AddObject(s1, tUrlStr.create(date, s2));
        //outl.Add(s1 + '/|' + s2+ ' ' + date);
      end;
    end;
  end;

initialization
  slr := tStringList.Create;
  slw := tStringList.Create;
  slw.CaseSensitive:=false;
  slw.Sorted:=true;

finalization
  slr.Destroy;
  slw.Destroy;

end.

