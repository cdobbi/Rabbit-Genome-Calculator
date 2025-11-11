CREATE DATABASE rabbit_genetics;

USE rabbit_genetics;

CREATE TABLE genetic_options (
    id INT AUTO_INCREMENT PRIMARY KEY,
    locus VARCHAR(50),
    option_text VARCHAR(255),
    genotype VARCHAR(50)
);

-- Insert sample data for color family (expand with all options)
INSERT INTO genetic_options (locus, option_text, genotype) VALUES
('family', '1. Full — C (CC)', 'CC'),
('family', '2. Chinchilla — c(chd) (cchdcchd)', 'cchdcchd'),
('family', '3. Seal — ch (chch)', 'chch'),
('family', '4. Sable — c(y) (cycy)', 'cycy'),
('family', '5. Himalayan — c(h) (cccc)', 'cccc'),
('family', '6. Ruby-Eyed-White — c (cc)', 'cc');

-- Insert sample data for color genotypes (expand with all options)
INSERT INTO genetic_options (locus, genotype) VALUES
('color', 'BB'),
('color', 'Bb'),
('color', 'bb'),
('color', 'BB'),
('color', 'bb'),
('color', 'BB'),
('color', 'bb'),
('color', 'BB'),
('color', 'Bb'),
('color', 'bb');
