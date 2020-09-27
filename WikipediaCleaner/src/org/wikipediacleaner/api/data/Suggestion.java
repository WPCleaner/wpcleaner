/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.utils.Configuration;


/**
 * Suggestions for text replacements.
 */
public class Suggestion implements Comparable<Suggestion> {

  private final static Logger log = LoggerFactory.getLogger(Suggestion.class);

  private final static String TAG_NOWIKI_1 = "<nowiki>";
  private final static String TAG_NOWIKI_2 = "</nowiki>";

  /**
   * Page and chapter in which the suggestion is defined.
   */
  private final String chapter;

  /**
   * Regular expression pattern.
   */
  private final Pattern pattern;

  /**
   *  True if the pattern is not a native WPCleaner pattern (AWB, ...)
   */
  private final boolean other;

  /**
   * List of possible replacements.
   */
  private final List<ElementarySuggestion> suggestions;

  /**
   * Comment for the replacements.
   */
  private String comment;

  /**
   * Clean a non WPCleaner pattern.
   * 
   * @param patternText Original pattern.
   * @return Cleaned up pattern.
   */
  public static String cleanPattern(String patternText) {
    if (patternText == null) {
      return null;
    }
    
    // Check for {{ or }}
    int lastIndex = 0;
    int currentIndex = 0;
    StringBuilder tmpPattern = new StringBuilder();
    while (currentIndex < patternText.length()) {
      if (patternText.startsWith("{{", currentIndex)) {
        if (currentIndex > lastIndex) {
          tmpPattern.append(patternText.substring(lastIndex, currentIndex));
        }
        tmpPattern.append("\\{\\{");
        currentIndex += 2;
        lastIndex = currentIndex;
      } else if (patternText.startsWith("}}", currentIndex)) {
        if (currentIndex > lastIndex) {
          tmpPattern.append(patternText.substring(lastIndex, currentIndex));
        }
        tmpPattern.append("\\}\\}}");
        currentIndex += 2;
        lastIndex = currentIndex;
      } else {
        currentIndex++;
      }
    }
    if (currentIndex > 0) {
      if (lastIndex < patternText.length()) {
        tmpPattern.append(patternText.substring(lastIndex));
      }
      patternText = tmpPattern.toString();
    }

    return patternText;
  }

  /**
   * Create a Suggestion.
   * 
   * @param patternText Search pattern.
   * @param other True if the pattern is not a native WPCleaner pattern.
   * @param chapter Page and chapter in which the suggestion is defined.
   * @return Suggestion or null if there's a problem.
   */
  public static Suggestion createSuggestion(
      String patternText, boolean other,
      String chapter) {
    /*log.debug(
        "Parsing {} pattern syntax {}:\n  [{}]",
        other ? "AWB" : "WPC",
        chapter != null ? "in " + chapter : "",
        patternText);*/
    try {
      if ((patternText.startsWith(TAG_NOWIKI_1)) &&
          (patternText.endsWith(TAG_NOWIKI_2))) {
        patternText = patternText.substring(
            TAG_NOWIKI_1.length(),
            patternText.length() - TAG_NOWIKI_2.length());
      }
      Pattern pattern = Pattern.compile(patternText);
      return new Suggestion(pattern, other, chapter);
    } catch (PatternSyntaxException e) {
      log.warn(
          "Incorrect {} pattern syntax {}:\n  {}",
          other ? "AWB" : "WPC",
          chapter != null ? "in " + chapter : "",
          e.getMessage());
    }
    return null;
  }

  /**
   * @param pattern Search pattern.
   * @param other True if the pattern is not a native WPCleaner pattern.
   * @param chapter Page and chapter in which the suggestion is defined.
   */
  private Suggestion(
      Pattern pattern, boolean other,
      String chapter) {
    this.chapter = chapter;
    this.pattern = pattern;
    this.other = other;
    this.suggestions = new ArrayList<ElementarySuggestion>();
    this.comment = null;
  }

  /**
   * @return Page and chapter in which the suggestion is defined.
   */
  public String getChapter() {
    return chapter;
  }

  /**
   * @return True if the pattern is not a native WPCleaner pattern.
   */
  public boolean isOtherPattern() {
    return other;
  }

  /**
   * @return Search pattern.
   */
  public String getPatternText() {
    return pattern.pattern();
  }

  /**
   * Add a possible replacement.
   * 
   * @param replacement Replacement.
   * @param automatic True if replacement can be done automatically.
   */
  public void addReplacement(String replacement, boolean automatic) {
    if (replacement != null) {
      if ((replacement.startsWith(TAG_NOWIKI_1)) &&
          (replacement.endsWith(TAG_NOWIKI_2))) {
        replacement = replacement.substring(
            TAG_NOWIKI_1.length(),
            replacement.length() - TAG_NOWIKI_2.length());
      }
      boolean added = false;
      for (ElementarySuggestion suggestion : suggestions) {
        if (replacement.equals(suggestion.getReplacement())) {
          added = true;
          if (automatic) {
            suggestion.setAutomatic();
          }
        }
      }
      if (!added) {
        suggestions.add(new ElementarySuggestion(replacement, automatic));
      }
    }
  }

  /**
   * @param comment Comment.
   */
  public void setComment(String comment) {
    this.comment = comment;
  }

  /**
   * @return Comment.
   */
  public String getComment() {
    return comment;
  }

  /**
   * @param text Text to look at.
   * @return A matcher for the pattern
   */
  public Matcher initMatcher(String text) {
    Matcher matcher = pattern.matcher(text);
    matcher.useAnchoringBounds(false);
    matcher.useTransparentBounds(true);
    return matcher;
  }

  /**
   * @return True if at least some replacements are automatic.
   */
  public boolean hasAutomaticReplacements() {
    if (suggestions != null) {
      for (ElementarySuggestion suggestion : suggestions) {
        if (suggestion.isAutomatic()) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * @param initialText Initial text.
   * @param begin Begin index.
   * @param end End index.
   * @return Possible replacements.
   */
  public List<ElementarySuggestion> getReplacements(
      String initialText, int begin, int end) {
    List<ElementarySuggestion> list = new ArrayList<ElementarySuggestion>();
    for (ElementarySuggestion suggestion : suggestions) {
      String replacement = suggestion.getReplacement();
      try {
        String newText = pattern.matcher(initialText).replaceFirst(replacement);
        String initialPrefix = initialText.substring(0, Math.min(begin, initialText.length()));
        String newPrefix = newText.substring(0, Math.min(begin, initialText.length()));
        String initialSuffix = initialText.substring(end);
        String newSuffix = newText.substring(Math.max(newText.length() - initialText.length() + end, 0));
        if (initialPrefix.equals(newPrefix) && initialSuffix.equals(newSuffix)) {
          newText = newText.substring(newPrefix.length(), newText.length() - newSuffix.length());
          boolean added = false;
          for (ElementarySuggestion element : list) {
            if (newText.equals(element.getReplacement())) {
              added = true;
            }
          }
          if (!added) {
            list.add(new ElementarySuggestion(newText, suggestion.isAutomatic()));
          }
        }
      } catch (Exception e) {
        log.error(
            "Error when trying to get replacement\n  " +
            initialText.substring(begin, end) + " → " + replacement +
            "\n  " + e.getMessage());
      }
    }
    return list;
  }

  // ==========================================================================
  // Chapters management
  // ==========================================================================

  /**
   * List of inactive chapters.
   */
  private static List<String> inactiveChapters;

  private final static Object lockClass = new Object();

  /**
   * Initialize list of inactive chapters.
   */
  private static void initializeInactiveChapters() {
    synchronized (lockClass) {
      if (inactiveChapters == null) {
        Configuration config = Configuration.getConfiguration();
        inactiveChapters = config.getStringList(null, Configuration.ARRAY_SPELLING_INACTIVE);
      }
    }
  }

  /**
   * @param chapter Chapter.
   * @return True if the chapter is active.
   */
  public static boolean isChapterActive(String chapter) {
    initializeInactiveChapters();
    return !inactiveChapters.contains(chapter);
  }

  /**
   * @param page Page.
   * @param title Title.
   * @return True if the chapter is active.
   */
  public static boolean isChapterActive(String page, String title) {
    initializeInactiveChapters();
    return !inactiveChapters.contains(page + "#" + title);
  }

  /**
   * @return True if the suggestion is active.
   */
  public boolean isActive() {
    initializeInactiveChapters();
    return isChapterActive(chapter);
  }

  /**
   * Activate or deactivate a list of chapters.
   * 
   * @param page Page.
   * @param chapters Chapters.
   * @param activate True to activate, false to deactivate.
   */
  public static void activateChapters(String page, List<String> chapters, boolean activate) {
    initializeInactiveChapters();
    if ((chapters == null) || (chapters.isEmpty())) {
      return;
    }
    for (String chapter : chapters) {
      String chapterName = (page != null ? page + "#" : "") + chapter;
      if (activate) {
        inactiveChapters.remove(chapterName);
      } else if (!inactiveChapters.contains(chapterName)) {
        inactiveChapters.add(chapterName);
        Collections.sort(inactiveChapters);
      }
    }
    Configuration config = Configuration.getConfiguration();
    config.setStringList(null, Configuration.ARRAY_SPELLING_INACTIVE, inactiveChapters);
  }

  /**
   * Construct a list of chapters containing suggestions.
   * 
   * @param suggestions List of suggestions.
   * @return List of chapters containing suggestions (key=Page, value=Chapters).
   */
  public static Map<String, List<String>> getChapters(Collection<Suggestion> suggestions) {
    Map<String, List<String>> chapters = new HashMap<String, List<String>>();
    for (Suggestion suggestion : suggestions) {
      String chapter = suggestion.getChapter();
      int sharpIndex = chapter.indexOf('#');
      String page = (sharpIndex < 0) ? chapter : chapter.substring(0, sharpIndex);
      String title = (sharpIndex < 0) ? "" : chapter.substring(sharpIndex + 1);
      List<String> pageChapters = chapters.get(page);
      if (pageChapters == null) {
        pageChapters = new ArrayList<String>();
        chapters.put(page, pageChapters);
      }
      if (!pageChapters.contains(title)) {
        pageChapters.add(title);
        Collections.sort(pageChapters);
      }
    }
    return chapters;
  }

  // ==========================================================================
  // Comparable interface
  // ==========================================================================

  /**
   * @param   o the object to be compared.
   * @return  a negative integer, zero, or a positive integer as this object
   *    is less than, equal to, or greater than the specified object.
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo(Suggestion o) {
    if (o == null) {
      return -1;
    }
    if (this.other != o.other) {
      return this.other ? 1 : -1;
    }
    if (this.comment != null) {
      if (o.comment == null) {
        return -1;
      }
      return this.comment.compareTo(o.comment);
    }
    if (o.comment != null) {
      return 1;
    }
    return 0;
  }

  /**
   * Bean for holding an elementary suggestion.
   */
  public static class ElementarySuggestion {

    /**
     * Replacement string.
     */
    private final String replacement;

    /**
     * True if the replacement can be done automatically.
     */
    private boolean automatic;

    /**
     * @param replacement Replacement string.
     * @param automatic True if the replacement can be done automatically.
     */
    public ElementarySuggestion(String replacement, boolean automatic) {
      this.replacement = replacement;
      this.automatic = automatic;
    }

    /**
     * @return Replacement string.
     */
    public String getReplacement() {
      return replacement;
    }

    /**
     * @return True if the replacement can be done automatically.
     */
    public boolean isAutomatic() {
      return automatic;
    }

    /**
     * Call to mark the replacement to be done automatically.
     */
    public void setAutomatic() {
      this.automatic = true;
    }
  }
}
