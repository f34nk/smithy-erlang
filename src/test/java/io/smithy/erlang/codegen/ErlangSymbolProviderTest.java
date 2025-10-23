package io.smithy.erlang.codegen;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class ErlangSymbolProviderTest {
    
    @Test
    public void testToErlangName() {
        // Test PascalCase to snake_case conversion
        assertEquals("weather_service", ErlangSymbolProvider.toErlangName("WeatherService"));
        assertEquals("get_current_weather", ErlangSymbolProvider.toErlangName("GetCurrentWeather"));
        assertEquals("temperature_unit", ErlangSymbolProvider.toErlangName("TemperatureUnit"));
        
        // Test handling of consecutive capitals
        assertEquals("http_request", ErlangSymbolProvider.toErlangName("HTTPRequest"));
        assertEquals("api_version", ErlangSymbolProvider.toErlangName("APIVersion"));
        
        // Test already lowercase
        assertEquals("test", ErlangSymbolProvider.toErlangName("test"));
        
        // Test with numbers
        assertEquals("http2protocol", ErlangSymbolProvider.toErlangName("HTTP2Protocol"));
    }
    
    @Test
    public void testReservedWords() {
        // Reserved words should have underscore appended
        assertEquals("end_", ErlangSymbolProvider.toErlangName("End"));
        assertEquals("case_", ErlangSymbolProvider.toErlangName("Case"));
        assertEquals("if_", ErlangSymbolProvider.toErlangName("If"));
        assertEquals("after_", ErlangSymbolProvider.toErlangName("After"));
    }
}
