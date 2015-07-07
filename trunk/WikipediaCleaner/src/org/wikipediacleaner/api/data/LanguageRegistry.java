/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.zip.GZIPInputStream;


/**
 * A registry for languages.
 * 
 * This is based on the data file <code>language-subtag-registry.txt.tgz</code>.
 * This file is retrieved from <a href="http://www.iana.org/assignments/language-subtag-registry">IANA</a>
 * and compressed with gzip.
 */
public class LanguageRegistry {

  /**
   * Lock object for initialization.
   */
  private final static Object lock = new Object();

  /**
   * List of languages.
   */
  private static List<LanguageRegistry.Language> languages;

  /**
   * List of scripts.
   */
  private static List<LanguageRegistry.Script> scripts;

  /**
   * List of regions.
   */
  private static List<LanguageRegistry.Region> regions;

  /**
   * List of variants.
   */
  private static List<LanguageRegistry.Variant> variants;

  /**
   * Constructor.
   */
  public LanguageRegistry() {
    try {
      initializeRegistry();
    } catch (IOException e) {
      // TODO
    }
  }

  /**
   * @return List of all languages.
   */
  public List<LanguageRegistry.Language> getLanguages() {
    return languages;
  }

  /**
   * @param code Language code.
   * @return Language with the specified code.
   */
  public LanguageRegistry.Language getLanguage(String code) {
    if (code == null) {
      return null;
    }
    for (LanguageRegistry.Language language : languages) {
      if (code.equalsIgnoreCase(language.getCode())) {
        return language;
      }
    }
    return null;
  }

  /**
   * @param language Language.
   * @return List of all scripts available for the specified language.
   */
  public List<LanguageRegistry.Script> getScripts(LanguageRegistry.Language language) {
    List<LanguageRegistry.Script> tmpScripts = new ArrayList<LanguageRegistry.Script>(scripts.size());
    for (LanguageRegistry.Script script : scripts) {
      if ((language == null) ||
          (language.getSuppressScript() == null) ||
          (!language.getSuppressScript().equalsIgnoreCase(script.getCode()))) {
        tmpScripts.add(script);
      }
    }
    return tmpScripts;
  }

  /**
   * @param code Script code.
   * @return Script with the specified code.
   */
  public LanguageRegistry.Script getScript(String code) {
    if (code == null) {
      return null;
    }
    for (LanguageRegistry.Script script : scripts) {
      if (code.equalsIgnoreCase(script.getCode())) {
        return script;
      }
    }
    return null;
  }

  /**
   * @return List of all regions.
   */
  public List<LanguageRegistry.Region> getRegions() {
    return regions;
  }

  /**
   * @param code Region code.
   * @return Region with the specified code.
   */
  public LanguageRegistry.Region getRegion(String code) {
    if (code == null) {
      return null;
    }
    for (LanguageRegistry.Region region : regions) {
      if (code.equalsIgnoreCase(region.getCode())) {
        return region;
      }
    }
    return null;
  }

  /**
   * @return List of all variants.
   */
  public List<LanguageRegistry.Variant> getVariants(String prefix) {
    List<LanguageRegistry.Variant> tmpVariants = new ArrayList<LanguageRegistry.Variant>();
    for (LanguageRegistry.Variant variant : variants) {
      if ((prefix == null) ||
          (variant.isPossibleForPrefix(prefix))) {
        tmpVariants.add(variant);
      }
    }
    return tmpVariants;
  }

  /**
   * @param code Variant code.
   * @return Variant with the specified code.
   */
  public LanguageRegistry.Variant getVariant(String code) {
    if (code == null) {
      return null;
    }
    for (LanguageRegistry.Variant variant : variants) {
      if (code.equalsIgnoreCase(variant.getCode())) {
        return variant;
      }
    }
    return null;
  }

  /**
   * Initialize registry.
   * @throws IOException 
   */
  private static void initializeRegistry() throws IOException {
    synchronized (lock) {
      if (languages == null) {
        // Open file containing the registry
        URL url = LanguageRegistry.class.getClassLoader().getResource(
            "org/wikipediacleaner/api/data/language-subtag-registry.txt.gz");
        if (url == null) {
          return;
        }
        BufferedReader reader = new BufferedReader(new InputStreamReader(
            new GZIPInputStream(url.openStream()), "UTF8"));

        List<LanguageRegistry.Language> tmpLanguages = new ArrayList<LanguageRegistry.Language>();
        List<LanguageRegistry.Script> tmpScripts = new ArrayList<LanguageRegistry.Script>();
        List<LanguageRegistry.Region> tmpRegions = new ArrayList<LanguageRegistry.Region>();
        List<LanguageRegistry.Variant> tmpVariants = new ArrayList<LanguageRegistry.Variant>();
        String line = null;
        while ((line = reader.readLine()) != null) {
          List<String> lines = new ArrayList<String>();
          while ((line != null) && (!"%%".equals(line.trim()))) {
            lines.add(line);
            line = reader.readLine();
          }
          if ("Type".equals(getElementName(0, lines))) {
            String type = getElementValue(0, lines);
            if ("language".equalsIgnoreCase(type)) {
              if ("Subtag".equalsIgnoreCase(getElementName(1, lines))) {
                Language language = new Language(getElementValue(1, lines));
                boolean shouldKeep = true;
                for (int lineNum = 2; lineNum < lines.size(); lineNum++) {
                  String name = getElementName(lineNum, lines);
                  String value = getElementValue(lineNum, lines);
                  if ("Description".equalsIgnoreCase(name)) {
                    language.addDescription(value);
                  } else if ("Comments".equalsIgnoreCase(name)) {
                    language.addComments(value);
                  } else if ("Suppress-Script".equalsIgnoreCase(name)) {
                    language.setSuppressScript(value);
                  } else if ("Scope".equalsIgnoreCase(name)) {
                    if ("special".equalsIgnoreCase(value)) {
                      shouldKeep = false;
                    }
                  } else if ("Deprecated".equalsIgnoreCase(name)) {
                    shouldKeep = false;
                  }
                }
                if (shouldKeep) {
                  tmpLanguages.add(language);
                }
              }
            } else if ("script".equalsIgnoreCase(type)) {
              if ("Subtag".equalsIgnoreCase(getElementName(1, lines))) {
                Script script = new Script(getElementValue(1, lines));
                boolean shouldKeep = true;
                for (int lineNum = 2; lineNum < lines.size(); lineNum++) {
                  String name = getElementName(lineNum, lines);
                  String value = getElementValue(lineNum, lines);
                  if ("Description".equalsIgnoreCase(name)) {
                    script.addDescription(value);
                  } else if("Comments".equalsIgnoreCase(name)) {
                    script.addComments(value);
                  }
                }
                if (shouldKeep) {
                  tmpScripts.add(script);
                }
              }
            } else if ("region".equalsIgnoreCase(type)) {
              if ("Subtag".equalsIgnoreCase(getElementName(1, lines))) {
                Region region = new Region(getElementValue(1, lines));
                boolean shouldKeep = true;
                for (int lineNum = 2; lineNum < lines.size(); lineNum++) {
                  String name = getElementName(lineNum, lines);
                  String value = getElementValue(lineNum, lines);
                  if ("Description".equalsIgnoreCase(name)) {
                    region.addDescription(value);
                  } else if ("Comments".equalsIgnoreCase(name)) {
                    region.addComments(value);
                  } else if ("Deprecated".equalsIgnoreCase(name)) {
                    shouldKeep = false;
                  }
                }
                if (shouldKeep) {
                  tmpRegions.add(region);
                }
              }
            } else if ("variant".equalsIgnoreCase(type)) {
              if ("Subtag".equalsIgnoreCase(getElementName(1, lines))) {
                Variant variant = new Variant(getElementValue(1, lines));
                boolean shouldKeep = true;
                for (int lineNum = 2; lineNum < lines.size(); lineNum++) {
                  String name = getElementName(lineNum, lines);
                  String value = getElementValue(lineNum, lines);
                  if ("Description".equalsIgnoreCase(name)) {
                    variant.addDescription(value);
                  } else if ("Comments".equalsIgnoreCase(name)) {
                    variant.addComments(value);
                  } else if ("Prefix".equalsIgnoreCase(name)) {
                    variant.addPrefix(value);
                  } else if ("Deprecated".equalsIgnoreCase(name)) {
                    shouldKeep = false;
                  }
                }
                if (shouldKeep) {
                  tmpVariants.add(variant);
                }
              }
            }
          }
        }
        Collections.sort(tmpLanguages);
        languages = Collections.unmodifiableList(tmpLanguages);
        Collections.sort(tmpScripts);
        scripts = Collections.unmodifiableList(tmpScripts);
        Collections.sort(tmpRegions);
        regions = Collections.unmodifiableList(tmpRegions);
        Collections.sort(tmpVariants);
        variants = Collections.unmodifiableList(tmpVariants);
      }
    }
  }

  /**
   * @param lineNum Current line.
   * @param lines Array of lines.
   * @return Element name if the line is defining an element.
   */
  private static String getElementName(int lineNum, List<String> lines) {
    if ((lines == null) || (lineNum >= lines.size())) {
      return null;
    }
    String line = lines.get(lineNum);
    if (line.startsWith(" ")) {
      return null;
    }
    int colonIndex = line.indexOf(':');
    if (colonIndex <= 0) {
      return null;
    }
    return line.substring(0, colonIndex);
  }

  /**
   * @param lineNum Current line.
   * @param lines Array of lines.
   * @return Element value if the line is defining an element.
   */
  private static String getElementValue(int lineNum, List<String> lines) {
    if ((lines == null) || (lineNum >= lines.size())) {
      return null;
    }
    String line = lines.get(lineNum);
    if (line.startsWith(" ")) {
      return null;
    }
    int colonIndex = line.indexOf(':');
    if (colonIndex <= 0) {
      return null;
    }
    String value = line.substring(colonIndex + 1).trim();
    while ((lineNum + 1 < lines.size()) &&
           (lines.get(lineNum + 1).startsWith(" "))) {
      lineNum++;
      value += " " + lines.get(lineNum).trim();
    }
    // TODO: Manage values on several lines
    return value;
  }

  /**
   * Base class for holding information.
   */
  public static abstract class Element implements Comparable<Element> {
    
    /**
     * Language code.
     */
    private final String code;

    /**
     * Description.
     */
    private String description;

    /**
     * Comments.
     */
    private String comments;

    /**
     * @param code Code.
     */
    Element(String code) {
      this.code = code;
    }

    /**
     * @return Language code.
     */
    public String getCode() {
      return code;
    }

    /**
     * @param desc Description.
     */
    void addDescription(String desc) {
      if (this.description == null) {
        this.description = desc;
      } else {
        this.description += ", " + desc;
      }
    }

    /**
     * @return Description.
     */
    public String getDescription() {
      return description;
    }

    /**
     * @param commentary Comments
     */
    void addComments(String commentary) {
      if (this.comments == null) {
        this.comments = commentary;
      } else {
        this.comments += ", " + commentary;
      }
    }

    /**
     * @return Comments
     */
    public String getComments() {
      return comments;
    }

    /**
     * @param o Other element.
     * @return  a negative integer, zero, or a positive integer as the code
     *    is less than, equal to, or greater than the code of the specified element.
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(Element o) {
      return code.compareTo(o.code);
    }

    /**
     * @return String representation of the language.
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
      if (description != null) {
        return code + " - " + description;
      }
      return code;
    }
  }

  /**
   * Bean for holding information about a language.
   */
  public static class Language extends Element {

    /**
     * Implicit script, non necessary.
     */
    private String suppressScript;

    /**
     * @param code Language code.
     */
    Language(String code) {
      super(code);
    }

    /**
     * @param script Implicit script.
     */
    void setSuppressScript(String script) {
      this.suppressScript = script;
    }

    /**
     * @return Implicit script.
     */
    String getSuppressScript() {
      return this.suppressScript;
    }
  }

  /**
   * Bean for holding information about a script.
   */
  public static class Script extends Element {

    /**
     * @param code Script code.
     */
    Script(String code) {
      super(code);
    }
  }

  /**
   * Bean for holding information about a region.
   */
  public static class Region extends Element {

    /**
     * @param code Region code.
     */
    Region(String code) {
      super(code);
    }
  }

  /**
   * Bean for holding information about a variant.
   */
  public static class Variant extends Element {

    /**
     * List of possible prefixes.
     */
    private final List<String> prefixes;

    /**
     * @param code Variant code.
     */
    Variant(String code) {
      super(code);
      this.prefixes = new ArrayList<String>();
    }

    /**
     * @param prefix Prefix.
     */
    void addPrefix(String prefix) {
      if (!prefixes.contains(prefix)) {
        prefixes.add(prefix);
      }
    }

    /**
     * @param prefix Prefix.
     * @return True if the variant is possible for the prefix.
     */
    boolean isPossibleForPrefix(String prefix) {
      return prefixes.contains(prefix);
    }
  }
}
