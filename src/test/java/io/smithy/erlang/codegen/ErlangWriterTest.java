package io.smithy.erlang.codegen;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class ErlangWriterTest {
    
    @Test
    public void testModuleDeclaration() {
        ErlangWriter writer = new ErlangWriter();
        writer.writeModule("test_module");
        
        String output = writer.toString();
        assertTrue(output.contains("-module(test_module)."));
    }
    
    @Test
    public void testExportDeclaration() {
        ErlangWriter writer = new ErlangWriter();
        writer.writeExport("function1/0", "function2/1", "function3/2");
        
        String output = writer.toString();
        assertTrue(output.contains("-export(["));
        assertTrue(output.contains("function1/0"));
        assertTrue(output.contains("function2/1"));
        assertTrue(output.contains("function3/2"));
    }
    
    @Test
    public void testRecordDefinition() {
        ErlangWriter writer = new ErlangWriter();
        writer.writeRecord("person", () -> {
            writer.writeInline("name :: binary(),");
            writer.write("");
            writer.writeInline("age :: integer()");
            writer.write("");
        });
        
        String output = writer.toString();
        assertTrue(output.contains("-record(person, {"));
        assertTrue(output.contains("name :: binary()"));
        assertTrue(output.contains("age :: integer()"));
        assertTrue(output.contains("})."));
    }
    
    @Test
    public void testFunctionDefinition() {
        ErlangWriter writer = new ErlangWriter();
        writer.writeFunction("add", "A, B", () -> {
            writer.write("A + B.");
        });
        
        String output = writer.toString();
        assertTrue(output.contains("add(A, B) ->"));
        assertTrue(output.contains("A + B."));
    }
    
    @Test
    public void testComment() {
        ErlangWriter writer = new ErlangWriter();
        writer.writeComment("This is a comment");
        
        String output = writer.toString();
        assertTrue(output.contains("%% This is a comment"));
    }
}
