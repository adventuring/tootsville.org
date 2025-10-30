/**
 * Test for AvatarBuilder.getPathForPattern
 * 
 * This tests that patterns with multiple variants (spots, patches, sparkles)
 * properly select random variants, and single patterns work correctly.
 */

// Mock Path2D if not available
if (typeof Path2D === 'undefined') {
    global.Path2D = function(path) {
        this.path = path;
    };
}

// Mock Tootsville namespace
if (typeof Tootsville === 'undefined') {
    global.Tootsville = {};
}

// Load the patterns
require('./Patterns.js');
// Load the AvatarBuilder
require('./AvatarBuilder.js');

describe('AvatarBuilder.getPathForPattern', () => {
    test('returns a Path2D object', () => {
        const result = Tootsville.AvatarBuilder.getPathForPattern('spots');
        expect(result).toBeInstanceOf(Path2D);
    });
    
    test('handles single-string patterns correctly', () => {
        const result = Tootsville.AvatarBuilder.getPathForPattern('lightning');
        expect(result).toBeInstanceOf(Path2D);
        expect(result.path).toBe(Tootsville.AvatarBuilder.patterns.lightning);
    });
    
    test('selects from array patterns (spots)', () => {
        const results = new Set();
        // Run multiple times to try to get different variants
        for (let i = 0; i < 50; i++) {
            const result = Tootsville.AvatarBuilder.getPathForPattern('spots');
            expect(result).toBeInstanceOf(Path2D);
            // Verify it's one of the valid spot patterns
            const isValid = Tootsville.AvatarBuilder.patterns.spots.includes(result.path);
            expect(isValid).toBe(true);
            results.add(result.path);
        }
        // With 3 spot variants and 50 iterations, we should get at least 2 different ones
        // (unless we're extremely unlucky with the RNG)
        expect(results.size).toBeGreaterThanOrEqual(2);
    });
    
    test('selects from array patterns (sparkles)', () => {
        const results = new Set();
        for (let i = 0; i < 50; i++) {
            const result = Tootsville.AvatarBuilder.getPathForPattern('sparkles');
            expect(result).toBeInstanceOf(Path2D);
            const isValid = Tootsville.AvatarBuilder.patterns.sparkles.includes(result.path);
            expect(isValid).toBe(true);
            results.add(result.path);
        }
        expect(results.size).toBeGreaterThanOrEqual(2);
    });
    
    test('selects from array patterns (patches)', () => {
        const results = new Set();
        for (let i = 0; i < 50; i++) {
            const result = Tootsville.AvatarBuilder.getPathForPattern('patches');
            expect(result).toBeInstanceOf(Path2D);
            const isValid = Tootsville.AvatarBuilder.patterns.patches.includes(result.path);
            expect(isValid).toBe(true);
            results.add(result.path);
        }
        expect(results.size).toBeGreaterThanOrEqual(2);
    });
    
    test('handles case-insensitive pattern names', () => {
        const result1 = Tootsville.AvatarBuilder.getPathForPattern('SPOTS');
        const result2 = Tootsville.AvatarBuilder.getPathForPattern('SpOtS');
        expect(result1).toBeInstanceOf(Path2D);
        expect(result2).toBeInstanceOf(Path2D);
    });
    
    test('falls back to spots for unknown patterns', () => {
        const result = Tootsville.AvatarBuilder.getPathForPattern('nonexistent');
        expect(result).toBeInstanceOf(Path2D);
        // Should be one of the spots variants
        const isSpot = Tootsville.AvatarBuilder.patterns.spots.includes(result.path);
        expect(isSpot).toBe(true);
    });
});

