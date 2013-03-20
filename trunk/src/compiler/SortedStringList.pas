unit SortedStringList;
{$I Compiler.inc}

interface

uses BeRoStream;

const MaxListSize=2147483647 div sizeof(pointer);

      EmptyStr:ansistring='';

type TPOINTER=pointer;

     TSortedStringListClass=class
      public
       Str:ansistring;
       Obj:TPOINTER;
       constructor Create;
       destructor Destroy; override;
     end;

     PSortedStringListArray=^TSortedStringListArray;
     TSortedStringListArray=array[0..MaxListSize-1] of TSortedStringListClass;

     TSortedStringList=class
      private
       FList:PSortedStringListArray;
       FCount,FSize:longint;
       FSorted:boolean;
       function GetItem(index:longint):ansistring;
       procedure SetItem(index:longint;Value:ansistring);
       function GetObject(index:longint):TPOINTER;
       procedure SetObject(index:longint;value:TPOINTER);
       procedure SetSorted(ASorted:boolean);
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       procedure AddStrings(From:TSortedStringList);
       function Add(Str:ansistring):longint;
       function AddObject(Str:ansistring;Obj:TPOINTER):longint;
       procedure Insert(index:longint;Str:ansistring);
       procedure InsertObject(index:longint;Str:ansistring;Obj:TPOINTER);
       procedure Delete(index:longint);
       function Remove(Str:ansistring):longint;
       function RemoveObject(Obj:TPOINTER):longint;
       function Find(Str:ansistring):longint;
       function IndexOf(Str:ansistring):longint;
       function IndexOfObject(Obj:TPOINTER):longint;
       procedure Exchange(Index1,Index2:longint);
       procedure SetCapacity(NewCapacity:longint);
       procedure SetCount(NewCount:longint);
       procedure LoadFromFile(const FileName:ansistring);
       procedure SaveToFile(const FileName:ansistring);
       procedure BeginUpdate;
       procedure EndUpdate;
       procedure Sort;
       property Count:longint read FCount;
       property Capacity:longint read FSize write SetCapacity;
       property Sorted:boolean read FSorted write SetSorted;
       property Strings[index:longint]:ansistring read GetItem write SetItem; default;
       property Objects[index:longint]:TPOINTER read GetObject write SetObject;
     end;


implementation

constructor TSortedStringListClass.Create;
begin
 inherited Create;
 Str:='';
end;

destructor TSortedStringListClass.Destroy;
begin
 Str:='';
 inherited Destroy;
end;

constructor TSortedStringList.Create;
begin
 inherited Create;
 FCount:=0;
 FSize:=0;
 FList:=nil;
 FSorted:=false;
 Clear;
end;

destructor TSortedStringList.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TSortedStringList.Clear;
var Counter:longint;
begin
 for Counter:=0 to fCount-1 do begin
  if assigned(fList [Counter]) then begin
   fList^[Counter].Free;
  end;
 end;
 FCount:=0;
 FSize:=0;
 REALLOCMEM(FList,0);
end;

procedure TSortedStringList.SetCapacity(NewCapacity:longint);
begin
 if (NewCapacity>=0) and (NewCapacity<MaxListSize) then begin
  REALLOCMEM(FList,NewCapacity*sizeof(TSortedStringListClass));
  FSize:=NewCapacity;
 end;
end;

procedure TSortedStringList.SetCount(NewCount:longint);
begin
 if (NewCount>=0) and (NewCount<MaxListSize) then begin
  if NewCount<FCount then begin
   FCount:=NewCount;
  end else if NewCount>FCount then begin
   if NewCount>FSize then begin
    SetCapacity(NewCount);
   end;
   if FCount<NewCount then begin
    FILLCHAR(FList^[FCount],(NewCount-FCount)*sizeof(TSortedStringListClass),0);
   end;
   FCount:=NewCount;
  end;
 end;
end;

procedure TSortedStringList.AddStrings(From:TSortedStringList);
var index:longint;
begin
 if assigned(From) then begin
  for index:=0 to From.Count-1 do begin
   AddObject(From.FList[index].Str,From.FList[index].Obj);
  end;
 end;
end;

function TSortedStringList.Add(Str:ansistring):longint;
begin
 result:=AddObject(Str,nil);
end;

function TSortedStringList.AddObject(Str:ansistring;Obj:TPOINTER):longint;
begin
 if FCount=FSize then begin
  if FSize>64 then begin
   inc(FSize,FSize div 4);
  end else if FSize>8 then begin
   inc(FSize,16);
  end else begin
   inc(FSize,4);
  end;
  REALLOCMEM(FList,FSize*sizeof(TSortedStringListClass));
 end;
 FList^[FCount]:=TSortedStringListClass.Create;
 FList^[FCount].Str:=Str;
 FList^[FCount].Obj:=Obj;
 result:=FCount;
 inc(FCount);
end;

procedure TSortedStringList.Insert(index:longint;Str:ansistring);
begin
 InsertObject(index,Str,nil);
end;

procedure TSortedStringList.InsertObject(index:longint;Str:ansistring;Obj:TPOINTER);
var I:longint;
begin
 if (index>=0) and (index<FCount) then begin
  SetCount(FCount+1);
  I:=FCount-1;
  while I>index do begin
   FList^[I]:=FList^[I-1];
   inc(I);
  end;
  FList^[index]:=TSortedStringListClass.Create;
  FList^[index].Str:=Str;
  FList^[index].Obj:=Obj;
 end else if index=FCount then begin
  AddObject(Str,Obj);
 end else if index>FCount then begin
  SetCount(index);
  AddObject(Str,Obj);
 end;
end;

procedure TSortedStringList.Delete(index:longint);
var I,J,K:longint;
begin
 if (index>=0) and (index<FCount) then begin
  if assigned(FList[index]) then FList[index].Free;
  K:=FCount-1;
  J:=index;
  I:=J;
  while I<K do begin
   FList^[I]:=FList^[I+1];
   inc(I);
  end;
  SetCount(K);
 end;
end;

function TSortedStringList.Remove(Str:ansistring):longint;
var I,J,K:longint;
begin
 result:=-1;
 K:=FCount;
 J:=-1;
 I:=0;
 while I<K do begin
  if assigned(FList^[I]) and (FList^[I].Str=Str) then begin
   J:=I;
   break;
  end;
  inc(I);
 end;
 if J>=0 then begin
  if assigned(FList[J]) then FList[J].Free;
  dec(K);
  I:=J;
  while I<K do begin
   FList^[I]:=FList^[I+1];
   inc(I);
  end;
  SetCount(K);
  result:=J;
 end;
end;

function TSortedStringList.RemoveObject(Obj:TPOINTER):longint;
var I,J,K:longint;
begin
 result:=-1;
 K:=FCount;
 J:=-1;
 I:=0;
 while I<K do begin
  if assigned(FList^[I]) and (FList^[I].Obj=Obj) then begin
   J:=I;
   break;
  end;
  inc(I);
 end;
 if J>=0 then begin
  if assigned(FList[J]) then FList[J].Free;
  dec(K);
  I:=J;
  while I<K do begin
   FList^[I]:=FList^[I+1];
   inc(I);
  end;
  SetCount(K);
  result:=J;
 end;
end;

function TSortedStringList.Find(Str:ansistring):longint;
var I:longint;
begin
 result:=-1;
 I:=0;
 while I<FCount do begin
  if assigned(FList^[I]) and (FList^[I].Str=Str) then begin
   result:=I;
   exit;
  end;
  inc(I);
 end;
end;

function TSortedStringList.IndexOf(Str:ansistring):longint;
var I:longint;
begin
 result:=-1;
 I:=0;
 while I<FCount do begin
  if assigned(FList^[I]) and (FList^[I].Str=Str) then begin
   result:=I;
   exit;
  end;
  inc(I);
 end;
end;

function TSortedStringList.IndexOfObject(Obj:TPOINTER):longint;
var I:longint;
begin
 result:=-1;
 I:=0;
 while I<FCount do begin
  if assigned(FList^[I]) and (FList^[I].Obj=Obj) then begin
   result:=I;
   exit;
  end;
  inc(I);
 end;
end;

procedure TSortedStringList.Exchange(Index1,Index2:longint);
var TempPointer:TSortedStringListClass;
begin
 if (Index1>=0) and (Index1<FCount) and (Index2>=0) and (Index2<FCount) then begin
  TempPointer:=FList^[Index1];
  FList^[Index1]:=FList^[Index2];
  FList^[Index2]:=TempPointer;
 end;
end;

function TSortedStringList.GetItem(index:longint):ansistring;
begin
 if (index>=0) and (index<FCount) then begin
  result:=FList^[index].Str;
 end else begin
  result:='';
 end;
end;

procedure TSortedStringList.SetItem(index:longint;Value:ansistring);
begin
 if (index>=0) and (index<FCount) then FList^[index].Str:=Value;
end;

function TSortedStringList.GetObject(index:longint):TPOINTER;
begin
 if (index>=0) and (index<FCount) then begin
  result:=FList^[index].Obj;
 end else begin
  result:=nil;
 end;
end;

procedure TSortedStringList.SetObject(index:longint;Value:TPOINTER);
begin
 if (index>=0) and (index<FCount) then FList^[index].Obj:=Value;
end;

procedure TSortedStringList.SetSorted(ASorted:boolean);
begin
 FSorted:=ASorted;
 if FSorted then Sort;
end;

procedure TSortedStringList.LoadFromFile(const FileName:ansistring);
var FileStream:TBeRoFileStream;
begin
 Clear;
 FileStream:=TBeRoFileStream.Create(FileName);
 while FileStream.Position<FileStream.Size do begin
  Add(FileStream.ReadLine);
 end;
 FileStream.Destroy;
end;

procedure TSortedStringList.SaveToFile(const FileName:ansistring);
var FileStream:TBeRoFileStream;
    Counter:longint;
begin
 FileStream:=TBeRoFileStream.CreateNew(FileName);
 for Counter:=0 to fCount-1 do begin
  if assigned(fList [Counter]) then begin
   FileStream.WriteLine(fList^[Counter].Str);
  end;
 end;
 FileStream.Destroy;
end;

procedure TSortedStringList.BeginUpdate;
begin
end;

procedure TSortedStringList.EndUpdate;
begin
end;

procedure TSortedStringList.Sort;
var Counter:longint;
begin
 Counter:=0;
 while Counter<fCount-1 do begin
  if assigned(fList[Counter]) and assigned(fList[Counter+1]) then begin
   if fList^[Counter+1].Str<fList^[Counter].Str then begin
    Exchange(Counter,Counter+1);
    Counter:=-1;
   end;
  end;
  inc(Counter);
 end;
end;

end.
