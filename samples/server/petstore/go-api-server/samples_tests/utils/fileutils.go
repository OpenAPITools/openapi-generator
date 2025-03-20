package utils

import (
	"bufio"
	"os"
	"strings"
)

// Load file content as array of lines
func ReadLines(filePath string) ([]string) {
	var lines []string

	file, err := os.Open(filePath)
	if err != nil {
		panic("Cannot open file " + filePath);
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		panic("Cannot scan file " + filePath);
	}

	return lines
}

// Load file content as string
func ReadContent(filePath string) (string) {
	file, err := os.Open(filePath)
	if err != nil {
		panic("Cannot open file " + filePath);
	}
	defer file.Close()

	var contentBuilder strings.Builder
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		contentBuilder.WriteString(scanner.Text() + "\n")
	}

	if err := scanner.Err(); err != nil {
		panic("Cannot scan file " + filePath);
	}

	return contentBuilder.String()
}