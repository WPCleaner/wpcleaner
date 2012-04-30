/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


/**
 * Suggestions for text replacements.
 */
public class Suggestion {

  private final static Log log = LogFactory.getLog(Suggestion.class);

  private final static String TAG_NOWIKI_1 = "<nowiki>";
  private final static String TAG_NOWIKI_2 = "</nowiki>";

  private final Pattern pattern;
  private final List<String> replacements;
  private String comment;

  /**
   * Create a Suggestion.
   * 
   * @param patternText Search pattern.
   * @return Suggestion or null if there's a problem.
   */
  public static Suggestion createSuggestion(String patternText) {
    try {
      if ((patternText.startsWith(TAG_NOWIKI_1)) &&
          (patternText.endsWith(TAG_NOWIKI_2))) {
        patternText = patternText.substring(
            TAG_NOWIKI_1.length(),
            patternText.length() - TAG_NOWIKI_2.length());
      }
      Pattern pattern = Pattern.compile(patternText);
      return new Suggestion(pattern);
    } catch (PatternSyntaxException e) {
      log.warn("Incorrect pattern syntax for [" + patternText + "]: " + e.getMessage());
    }
    return null;
  }

  /**
   * @param pattern Search pattern.
   */
  private Suggestion(Pattern pattern) {
    this.pattern = pattern;
    this.replacements = new ArrayList<String>();
    this.comment = null;
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
   */
  public void addReplacement(String replacement) {
    if (replacement != null) {
      if ((replacement.startsWith(TAG_NOWIKI_1)) &&
          (replacement.endsWith(TAG_NOWIKI_2))) {
        replacement = replacement.substring(
            TAG_NOWIKI_1.length(),
            replacement.length() - TAG_NOWIKI_2.length());
      }
      replacements.add(replacement);
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
   * @param initialText Initial text.
   * @return Possible replacements.
   */
  public List<String> getReplacements(String initialText) {
    List<String> list = new ArrayList<String>();
    for (String replacement : replacements) {
      try {
        list.add(pattern.matcher(initialText).replaceFirst(replacement));
      } catch (Exception e) {
        log.error("Unable to get replacement", e);
      }
    }
    return list;
  }
}
