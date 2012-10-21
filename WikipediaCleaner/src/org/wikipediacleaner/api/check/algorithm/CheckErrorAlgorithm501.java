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

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.regex.Matcher;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.NullActionProvider;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementAreas;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.Suggestion;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueInteger;
import org.wikipediacleaner.utils.Performance;


/**
 * Algorithm for analyzing error 501 of check wikipedia project.
 * Error 501: Spelling and typography
 */
public class CheckErrorAlgorithm501 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm501() {
    super("Spelling and typography");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors) {
    boolean result = false;
    if ((pageAnalysis == null) || (!pageAnalysis.shouldCheckSpelling())) {
      return result;
    }

    // Initialize active suggestions
    Map<String, Suggestion> suggestions = pageAnalysis.getWPCConfiguration().getSuggestions();
    if ((suggestions == null) || (suggestions.isEmpty())) {
      return result;
    }
    List<Suggestion> activeSuggestions = new LinkedList<Suggestion>();
    for (Suggestion suggestion : suggestions.values()) {
      if (suggestion.isActive()) {
        activeSuggestions.add(suggestion);
      }
    }
    if (activeSuggestions.isEmpty()) {
      return result;
    }
    Configuration config = Configuration.getConfiguration();
    int slowRegexp = config.getInt(null, ConfigurationValueInteger.SLOW_REGEXP);

    // Check spelling in templates
    List<Replacement> replacements = new ArrayList<Replacement>();
    if ((result == false) || (errors != null)) {
      result |= analyzeTemplates(pageAnalysis, activeSuggestions, replacements);
    }

    // Check spelling in internal links
    if ((result == false) || (errors != null)) {
      result |= analyzeInternalLinks(pageAnalysis, activeSuggestions, replacements);
    }

    // Check spelling in normal text with non native regular expressions
    if ((result == false) || (errors != null)) {
      result |= analyzeNonNativeText(pageAnalysis, activeSuggestions, replacements, slowRegexp);
    }

    // Check spelling in normal text with native regular expressions
    if ((result == false) || (errors != null)) {
      result |= analyzeNativeText(pageAnalysis, activeSuggestions, replacements, slowRegexp);
    }

    // Analyze replacements
    if (errors != null) {
      String contents = pageAnalysis.getContents();
      Collections.sort(replacements);
      ReplacementComparator comparator = new ReplacementComparator();
      while (!replacements.isEmpty()) {
        // Analyze group of replacements
        List<Replacement> group = getFirstGroup(replacements);
        Collections.sort(group, comparator);
        int minBegin = Integer.MAX_VALUE;
        int maxEnd = 0;
        for (Replacement replacement : group) {
          minBegin = Math.min(minBegin, replacement.getBegin());
          maxEnd = Math.max(maxEnd, replacement.getEnd());
        }
  
        // Create error
        CheckErrorResult error = createCheckErrorResult(
            pageAnalysis.getPage(), minBegin, maxEnd);
        String previousComment = null;
        List<String> alreadyAdded = new ArrayList<String>();
        for (Replacement replacement : group) {

          // Construct replacement
          int begin = replacement.getBegin();
          int end = replacement.getEnd();
          String newText =
              contents.substring(minBegin, begin) +
              replacement.getReplacement() +
              contents.substring(end, maxEnd);

          if (!alreadyAdded.contains(newText)) {
            // Manage comment
            String comment = replacement.getComment();
            if ((comment != null) &&
                (!comment.equals(previousComment))) {
              error.addPossibleAction(comment, new NullActionProvider());
            }
            previousComment = comment;

            error.addReplacement(newText);
            alreadyAdded.add(newText);
          }
        }
        errors.add(error);
      }
    }

    return result;
  }

  /**
   * Check spelling in normal text with native regular expressions.
   * 
   * @param analysis Page analysis.
   * @param suggestions Active suggestions.
   * @param replacements List of possible replacements.
   * @param slowRegexp Threshold for slow regular expression.
   * @return True if an error has been found.
   */
  private boolean analyzeNativeText(
      PageAnalysis analysis, List<Suggestion> suggestions,
      List<Replacement> replacements, int slowRegexp) {
    boolean result = false;

    // Check every suggestion
    List<ContentsChunck> chuncks = computeContentsChuncks(analysis);
    String contents = analysis.getContents();
    Iterator<Suggestion> itSuggestion = suggestions.iterator();
    while (itSuggestion.hasNext()) {
      Suggestion suggestion = itSuggestion.next();
      if (!suggestion.isOtherPattern()) {
        Performance perf = new Performance("Slow regular expression: " + suggestion.getPatternText());
        perf.setThreshold(slowRegexp);
        itSuggestion.remove();
        Matcher matcher = suggestion.initMatcher(contents);
        for (ContentsChunck chunck : chuncks) {
          matcher.region(chunck.getBegin(), chunck.getEnd());
          while (matcher.find()) {
            int begin = matcher.start();
            int end = matcher.end();
            if ((begin == 0) ||
                (!Character.isLetterOrDigit(contents.charAt(begin))) ||
                (!Character.isLetterOrDigit(contents.charAt(begin - 1)))) {
              if ((end >= contents.length()) ||
                  (!Character.isLetterOrDigit(contents.charAt(end))) ||
                  (!Character.isLetterOrDigit(contents.charAt(end - 1)))) {
                result |= addReplacements(begin, end, contents, suggestion, replacements);
              }
            }
          }
        }
        perf.printEnd();
      }
    }

    return result;
  }

  /**
   * Check spelling in normal text with non native regular expressions.
   * 
   * @param analysis Page analysis.
   * @param suggestions Active suggestions.
   * @param replacements List of possible replacements.
   * @param slowRegexp Threshold for slow regular expression.
   * @return True if an error has been found.
   */
  private boolean analyzeNonNativeText(
      PageAnalysis analysis, List<Suggestion> suggestions,
      List<Replacement> replacements, int slowRegexp) {
    boolean result = false;

    // Check every suggestion
    List<ContentsChunck> chuncks = computeContentsChuncks(analysis);
    String contents = analysis.getContents();
    Iterator<Suggestion> itSuggestion = suggestions.iterator();
    while (itSuggestion.hasNext()) {
      Suggestion suggestion = itSuggestion.next();
      if (suggestion.isOtherPattern()) {
        Performance perf = new Performance("Slow " + suggestion.getComment() + ":" + suggestion.getPatternText());
        perf.setThreshold(slowRegexp);
        itSuggestion.remove();
        Matcher matcher = suggestion.initMatcher(contents);
        for (ContentsChunck chunck : chuncks) {
          matcher.region(chunck.getBegin(), chunck.getEnd());
          while (matcher.find()) {
            int begin = matcher.start();
            int end = matcher.end();
            boolean keep = true;
            // Remove texts between dots (potential URL)
            if ((begin > 1) &&
                (contents.charAt(begin - 1) == '.') &&
                (Character.isLetterOrDigit(contents.charAt(begin - 2))) &&
                (end + 1 < contents.length()) &&
                (contents.charAt(end) == '.') &&
                (Character.isLetterOrDigit(contents.charAt(end + 1)))) {
              keep = false;
            }
            if (keep) {
              result |= addReplacements(begin, end, contents, suggestion, replacements);
            }
          }
        }
        perf.printEnd();
      }
    }

    return result;
  }

  /**
   * Check spelling in templates.
   * 
   * @param analysis Page analysis.
   * @param suggestions Active suggestions.
   * @param replacements List of possible replacements.
   * @return True if an error has been found.
   */
  private boolean analyzeTemplates(
      PageAnalysis analysis, List<Suggestion> suggestions,
      List<Replacement> replacements) {
    boolean result = false;

    // Check every suggestion
    List<PageElementTemplate> templates = analysis.getTemplates();
    String contents = analysis.getContents();
    int contentsLength = contents.length();
    Iterator<Suggestion> itSuggestion = suggestions.iterator();
    while (itSuggestion.hasNext()) {
      Suggestion suggestion = itSuggestion.next();
      if (suggestion.getPatternText().startsWith("\\{\\{")) {
        itSuggestion.remove();
        Matcher matcher = suggestion.initMatcher(contents);

        // Check suggestion on every template
        for (PageElementTemplate template : templates) {
          int begin = template.getBeginIndex();
          if (matcher.region(begin, contentsLength).lookingAt()) {
            int end = matcher.end();
            if ((end >= contentsLength) ||
                (!Character.isLetterOrDigit(contents.charAt(end))) ||
                (!Character.isLetterOrDigit(contents.charAt(end - 1)))) {
              result |= addReplacements(begin, end, contents, suggestion, replacements);
            }
          }
        }
      }
    }

    return result;
  }

  /**
   * Check spelling in internal links.
   * 
   * @param analysis Page analysis.
   * @param suggestions Active suggestions.
   * @param replacements List of possible replacements.
   * @return True if an error has been found.
   */
  private boolean analyzeInternalLinks(
      PageAnalysis analysis, List<Suggestion> suggestions,
      List<Replacement> replacements) {
    boolean result = false;

    // Check every suggestion
    List<PageElementInternalLink> links = analysis.getInternalLinks();
    String contents = analysis.getContents();
    int contentsLength = contents.length();
    Iterator<Suggestion> itSuggestion = suggestions.iterator();
    while (itSuggestion.hasNext()) {
      Suggestion suggestion = itSuggestion.next();
      if (suggestion.getPatternText().startsWith("\\[\\[")) {
        itSuggestion.remove();
        Matcher matcher = suggestion.initMatcher(contents);

        // Check suggestion on every internal links
        for (PageElementInternalLink link : links) {
          int begin = link.getBeginIndex();
          if (matcher.region(begin, contentsLength).lookingAt()) {
            int end = matcher.end();
            if ((end >= contentsLength) ||
                (!Character.isLetterOrDigit(contents.charAt(end))) ||
                (!Character.isLetterOrDigit(contents.charAt(end - 1)))) {
              result |= addReplacements(begin, end, contents, suggestion, replacements);
            }
          }
        }
      }
    }

    return result;
  }

  /**
   * Memorize possible replacements for a text.
   * 
   * @param begin Begin index of the initial text.
   * @param end End index of the initial text.
   * @param contents Current contents.
   * @param suggestion Suggestion.
   * @param replacements List of replacements.
   * @return True if a replacement has been added.
   */
  private boolean addReplacements(
      int begin, int end, String contents,
      Suggestion suggestion, List<Replacement> replacements) {
    boolean result = false;
    String text = contents.substring(begin, end);
    List<String> possibles = suggestion.getReplacements(text);
    if (possibles != null) {
      for (String possible : possibles) {
        if (!text.equals(possible)) {
          Replacement replacement = new Replacement(
              begin, end,
              suggestion.getComment(), suggestion.isOtherPattern(),
              possible);
          replacements.add(replacement);
          result = true;
        }
      }
    }
    return result;
  }

  /**
   * Retrieve first group of replacements.
   * 
   * @param replacements List of replacements.
   * @return First group of replacements.
   */
  private List<Replacement> getFirstGroup(List<Replacement> replacements) {
    List<Replacement> firstGroup = new LinkedList<Replacement>();
    Replacement replacement = replacements.get(0);
    int end = replacement.getEnd();
    firstGroup.add(replacement);
    replacements.remove(0);
    while (!replacements.isEmpty() && (replacements.get(0).getBegin() < end)) {
      replacement = replacements.get(0);
      end = Math.max(end, replacement.getEnd());
      firstGroup.add(replacement);
      replacements.remove(0);
    }
    return firstGroup;
  }

  /**
   * Split contents into analyzable chuncks.
   * 
   * @param analysis Page analysis.
   * @return List of contents chuncks.
   */
  private List<ContentsChunck> computeContentsChuncks(PageAnalysis analysis) {
    String contents = analysis.getContents();
    List<ContentsChunck> chuncks = new LinkedList<ContentsChunck>();
    chuncks.add(new ContentsChunck(0, contents.length()));

    // Remove templates
    List<PageElementTemplate> templates = analysis.getTemplates();
    for (PageElementTemplate template : templates) {
      removeArea(chuncks, template.getBeginIndex(), template.getEndIndex());
    }

    // Remove tags
    // TODO: Be more precise, analyze image descriptions
    removeCompleteTags(chuncks, analysis, PageElementTag.TAG_WIKI_GALLERY);
    removeCompleteTags(chuncks, analysis, PageElementTag.TAG_WIKI_MATH);
    removeCompleteTags(chuncks, analysis, PageElementTag.TAG_WIKI_CODE);
    removeCompleteTags(chuncks, analysis, PageElementTag.TAG_WIKI_TIMELINE);

    // Remove areas
    PageElementAreas areas = analysis.getAreas();
    for (PageElementAreas.Area area : areas.getAreas()) {
      removeArea(chuncks, area.getBeginIndex(), area.getEndIndex());
    }

    // Remove empty chuncks
    Iterator<ContentsChunck> itChuncks = chuncks.iterator();
    while (itChuncks.hasNext()) {
      ContentsChunck chunck = itChuncks.next();
      int begin = chunck.getBegin();
      int end = chunck.getEnd();
      String chunckContents = contents.substring(begin, end);
      int length = chunckContents.length();
      int currentIndex = 0;
      while ((currentIndex < length) &&
             (Character.isWhitespace(chunckContents.charAt(currentIndex)))) {
        currentIndex++;
      }
      if (currentIndex >= length) {
        itChuncks.remove();
      }
    }

    return chuncks;
  }

  /**
   * Remove complete tags from the list of chuncks of text.
   * 
   * @param chuncks List of chuncks of text.
   * @param analysis Page analysis.
   * @param tagName Tag name to remove.
   */
  private void removeCompleteTags(List<ContentsChunck> chuncks, PageAnalysis analysis, String tagName) {
    List<PageElementTag> tags = analysis.getCompleteTags(tagName);
    for (PageElementTag tag : tags) {
      removeArea(chuncks, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
    }
  }

  /**
   * Remove an area from the list of chuncks of text.
   * 
   * @param chuncks List of chuncks of text.
   * @param begin Begin of the area to remove.
   * @param end End of the area to remove.
   */
  private void removeArea(List<ContentsChunck> chuncks, int begin, int end) {
    ListIterator<ContentsChunck> itChuncks = chuncks.listIterator();
    while (itChuncks.hasNext()) {
      ContentsChunck chunck = itChuncks.next();
      if ((begin >= chunck.getEnd()) || (end <= chunck.getBegin())) {
        // Nothing to do
      } else {
        itChuncks.remove();
        if (begin > chunck.getBegin()) {
          itChuncks.add(new ContentsChunck(chunck.getBegin(), begin));
        }
        if (end < chunck.getEnd()) {
          itChuncks.add(new ContentsChunck(end, chunck.getEnd()));
        }
      }
    }
  }

  /**
   * Utility class to manage chuncks of text.
   */
  private static class ContentsChunck {
    private final int begin;
    private final int end;

    public ContentsChunck(int begin, int end) {
      this.begin = begin;
      this.end = end;
    }

    public int getBegin() {
      return begin;
    }

    public int getEnd() {
      return end;
    }
  }

  /**
   * Utility class to memorize possible replacements.
   */
  private static class Replacement implements Comparable<Replacement> {
    private final int begin;
    private final int end;
    private final String comment;
    private final boolean otherPattern;
    private final String replacement;

    public Replacement(
        int begin, int end,
        String comment, boolean otherPattern,
        String replacement) {
      this.begin = begin;
      this.end = end;
      this.otherPattern = otherPattern;
      this.comment = comment;
      this.replacement = replacement;
    }

    public int getBegin() {
      return begin;
    }

    public int getEnd() {
      return end;
    }

    public String getComment() {
      return comment;
    }

    public boolean isOtherPattern() {
      return otherPattern;
    }

    public String getReplacement() {
      return replacement;
    }

    /**
     * @param o
     * @return
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    public int compareTo(Replacement o) {
      if (begin != o.begin) {
        return (begin < o.begin ? -1 : 1);
      }
      if (end != o.end) {
        return (end < o.end ? -1 : 1);
      }
      return 0;
    }
  }

  /**
   * Utility class to sort Replacement in a group.
   */
  private static class ReplacementComparator implements Comparator<Replacement> {

    /**
     * Constructor.
     */
    public ReplacementComparator() {
    }

    /**
     * @param o1
     * @param o2
     * @return
     * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
     */
    public int compare(Replacement o1, Replacement o2) {
      
      // Comparison on native pattern
      if (o1.isOtherPattern() != o2.isOtherPattern()) {
        return (o1.isOtherPattern() ? 1 : -1);
      }

      // Comparison on begin
      if (o1.getBegin() != o2.getBegin()) {
        return (o1.getBegin() < o2.getBegin() ? -1 : 1);
      }

      // Comparison on comments
      if (o1.getComment() == null) {
        if (o2.getComment() != null) {
          return 1;
        }
      } else if (o2.getComment() == null) {
        return -1;
      } else {
        int compare = o1.getComment().compareTo(o2.getComment());
        if (compare != 0) {
          return compare;
        }
      }

      // Comparison on end
      if (o2.getEnd() != o2.getEnd()) {
        return (o1.getEnd() < o2.getEnd() ? -1 : 1);
      }

      return 0;
    }
  }
}
