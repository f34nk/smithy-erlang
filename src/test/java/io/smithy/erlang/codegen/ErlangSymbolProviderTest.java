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
        assertEquals("http2_protocol", ErlangSymbolProvider.toErlangName("HTTP2Protocol"));
    }
    
    @Test
    public void testDigitsInNames() {
        // Test AWS service names with digits (digit-uppercase transition)
        assertEquals("s3_storage", ErlangSymbolProvider.toErlangName("S3Storage"));
        assertEquals("ec2_instance", ErlangSymbolProvider.toErlangName("EC2Instance"));
        assertEquals("r2_storage", ErlangSymbolProvider.toErlangName("R2Storage"));
        assertEquals("base64_encoder", ErlangSymbolProvider.toErlangName("Base64Encoder"));
        
        // Test lowercase names with digits (should NOT insert underscores)
        assertEquals("s3", ErlangSymbolProvider.toErlangName("s3"));
        assertEquals("ec2", ErlangSymbolProvider.toErlangName("ec2"));
        assertEquals("base64", ErlangSymbolProvider.toErlangName("base64"));
        
        // Test mixed cases
        // Note: Complex acronyms like AWSS3Client can't be perfectly separated
        // The regex produces reasonable output: awss3_client
        assertEquals("awss3_client", ErlangSymbolProvider.toErlangName("AWSS3Client"));
        assertEquals("http2_connection", ErlangSymbolProvider.toErlangName("HTTP2Connection"));
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
