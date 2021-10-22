import Type from "./Type";

export enum SymbolType{

    VARIABLE,
    FUNCTION,
    METHOD,
    VECTOR,
    LIST,
    PARAMETER
}

export default class Symbol{

    public symbolType : SymbolType;

    public type: Type;
    public id : string;
    public value : any;

    constructor(symbolType : SymbolType, type : Type, id : string, value : any){
        this.symbolType = symbolType;
        this.type = type;
        this.id = id;
        this.value = value;
    }

    setValue(value : any){
        this.value = value;
    }

}
