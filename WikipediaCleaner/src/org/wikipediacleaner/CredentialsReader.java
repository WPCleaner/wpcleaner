package org.wikipediacleaner;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public final class CredentialsReader {

  private final static Logger LOGGER = LoggerFactory.getLogger(CredentialsReader.class);

  private CredentialsReader() {
    // Utility class
  }

  public static Credentials readCredentials(final String file) {
    String username = null;
    String password = null;
    try {
      final List<String> lines = Files.readAllLines(Path.of(file), StandardCharsets.UTF_8);
      for (String line : lines) {
        if (line.startsWith("user=")) {
          username = line.substring("user=".length());
        }
        if (line.startsWith("password=")) {
          password = line.substring("password=".length());
        }
      }
    } catch (IOException e) {
      LOGGER.error("Unable to load credentials file {}", file, e);
    }
    return new Credentials(username, password);
  }

  public record Credentials(String username, String password) {}
}
