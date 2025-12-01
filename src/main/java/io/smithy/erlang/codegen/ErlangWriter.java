package io.smithy.erlang.codegen;

import software.amazon.smithy.utils.CodeWriter;

public final class ErlangWriter extends CodeWriter {
    
    public ErlangWriter() {
        super();
        // No custom formatters for now - keep it simple
    }
    
    public ErlangWriter writeModule(String moduleName) {
        write("-module($L).", moduleName);
        return this;
    }
    
    public ErlangWriter writeExport(String... functions) {
        if (functions.length == 0) {
            write("-export([]).");
            return this;
        }
        
        write("-export([" + functions[0] + ",");
        for (int i = 1; i < functions.length; i++) {
            if (i < functions.length - 1) {
                write("         " + functions[i] + ",");
            } else {
                write("         " + functions[i]);
            }
        }
        write("        ]).");
        return this;
    }
    
    public ErlangWriter writeRecord(String recordName, Runnable fieldWriter) {
        write("-record($L, {", recordName);
        indent();
        fieldWriter.run();
        dedent();
        write("}).");
        write("");
        return this;
    }
    
    public ErlangWriter writeType(String typeName, String typeSpec) {
        write("-type $L() :: $L.", typeName, typeSpec);
        return this;
    }
    
    public ErlangWriter writeSpec(String functionName, String spec) {
        write("-spec $L$L.", functionName, spec);
        return this;
    }
    
    public ErlangWriter writeFunction(String name, String params, Runnable body) {
        write("$L($L) ->", name, params);
        indent();
        body.run();
        dedent();
        write("");
        return this;
    }
    
    public ErlangWriter writeComment(String comment) {
        write("%% " + comment);
        return this;
    }
    
    public ErlangWriter writeDocComment(String doc) {
        for (String line : doc.split("\n")) {
            write("%% " + line);
        }
        return this;
    }
}
