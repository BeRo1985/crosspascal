unit FastStringList;
{$I Compiler.inc}

interface

uses BeRoStream,BeRoStringHashMap,List;

const MaxListSize=2147483647 div SizeOf(pointer);

      EmptyStr:ansistring='';

type TPointer=pointer;
     TPointerS=array of TPointer;

     TFastStringListClass=class
      public
       Str:ansistring;
       PointerList:TList;
       constructor Create;
       destructor Destroy; override;
     end;

     PFastStringListArray=^TFastStringListArray;
     TFastStringListArray=array[0..MaxListSize-1] of TFastStringListClass;

     TFastStringList=class
      private
       FList:PFastStringListArray;
       FCount,FSize:longint;
       FHashMap:TBeRoStringHashMap;
       function GetItem(index:longint):ansistring;
       procedure SetItem(index:longint;Value:ansistring);
       function GetObject(index:longint):TList;
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function Add(const Str:ansistring):longint;
       function AddObject(const Str:ansistring;Obj:TPointer):longint;
       function Find(const Str:ansistring):longint;
       function IndexOf(const Str:ansistring):longint;
       function IndexOfObject(Obj:TPointer):longint;
       procedure SetCapacity(NewCapacity:longint);
       procedure SetCount(NewCount:longint);
       procedure BeginUpdate;
       procedure EndUpdate;
       property Count:longint read FCount;
       property Capacity:longint read FSize write SetCapacity;
       property Strings[index:longint]:ansistring read GetItem write SetItem; default;
       property Objects[index:longint]:TList read GetObject;
     end;


implementation

constructor TFastStringListClass.Create;
begin
 inherited Create;
 Str:='';
 PointerList:=TList.Create;
end;

destructor TFastStringListClass.Destroy;
begin
 Str:='';
 PointerList.Free;
 inherited Destroy;
end;

constructor TFastStringList.Create;
begin
 inherited Create;
 FHashMap:=TBeRoStringHashMap.Create;
 FCount:=0;
 FSize:=0;
 FList:=nil;
 Clear;
end;

destructor TFastStringList.Destroy;
begin
 Clear;
 FHashMap.Destroy;
 inherited Destroy;
end;

procedure TFastStringList.Clear;
var Counter:longint;
begin
 for Counter:=0 to fCount-1 do begin
  if assigned(fList [Counter]) then begin
   fList^[Counter].Free;
  end;
 end;
 FCount:=0;
 FSize:=0;
 ReallocMem(FList,0);
 FHashMap.Clear;
end;

procedure TFastStringList.SetCapacity(NewCapacity:longint);
begin
 if (NewCapacity>=0) and (NewCapacity<MaxListSize) then begin
  ReallocMem(FList,NewCapacity*SizeOf(TFastStringListClass));
  FSize:=NewCapacity;
 end;
end;

procedure TFastStringList.SetCount(NewCount:longint);
begin
 if (NewCount>=0) and (NewCount<MaxListSize) then begin
  if NewCount<FCount then begin
   FCount:=NewCount;
  end else if NewCount>FCount then begin
   if NewCount>FSize then begin
    SetCapacity(NewCount);
   end;
   if FCount<NewCount then begin
    FillChar(FList^[FCount],(NewCount-FCount)*SizeOf(TFastStringListClass),0);
   end;
   FCount:=NewCount;
  end;
 end;
end;

function TFastStringList.Add(const Str:ansistring):longint;
begin
 result:=AddObject(Str,nil);
end;

function TFastStringList.AddObject(const Str:ansistring;Obj:TPointer):longint;
var StringHashMapEntity:PBeRoStringHashMapEntity;
begin
 StringHashMapEntity:=FHashMap.Get(Str);
 if assigned(StringHashMapEntity) then begin
  result:=StringHashMapEntity.Value;
  if assigned(Obj) then begin
   FList^[result].PointerList.Add(Obj);
  end;
 end else begin
  if FCount=FSize then begin
   if FSize>64 then begin
    inc(FSize,FSize div 4);
   end else if FSize>8 then begin
    inc(FSize,16);
   end else begin
    inc(FSize,4);
   end;
   ReallocMem(FList,FSize*SizeOf(TFastStringListClass));
  end;
  FList^[FCount]:=TFastStringListClass.Create;
  FList^[FCount].Str:=Str;
  if assigned(Obj) then begin
   FList^[FCount].PointerList.Add(Obj);
  end;
  result:=FCount;
  FHashMap.Add(Str,result);
  inc(FCount);
 end;
end;

function TFastStringList.Find(const Str:ansistring):longint;
var StringHashMapEntity:PBeRoStringHashMapEntity;
begin
 StringHashMapEntity:=FHashMap.Get(Str);
 if assigned(StringHashMapEntity) then begin
  result:=StringHashMapEntity.Value;
 end else begin
  result:=-1;
 end;
end;

function TFastStringList.IndexOf(const Str:ansistring):longint;
begin
 result:=Find(Str);
end;

function TFastStringList.IndexOfObject(Obj:TPointer):longint;
var I:longint;
begin
 result:=-1;
 I:=0;
 while I<FCount do begin
  if assigned(FList^[I]) and (FList^[I].PointerList.Find(Obj)>=0) then begin
   result:=I;
   exit;
  end;
  inc(I);
 end;
end;

function TFastStringList.GetItem(index:longint):ansistring;
begin
 if (index>=0) and (index<FCount) then begin
  result:=FList^[index].Str;
 end else begin
  result:='';
 end;
end;

procedure TFastStringList.SetItem(index:longint;Value:ansistring);
begin
 if (index>=0) and (index<FCount) then FList^[index].Str:=Value;
end;

function TFastStringList.GetObject(index:longint):TList;
begin
 if (index>=0) and (index<FCount) then begin
  result:=FList^[index].PointerList;
 end else begin
  result:=nil;
 end;
end;

procedure TFastStringList.BeginUpdate;
begin
end;

procedure TFastStringList.EndUpdate;
begin
end;

end.
