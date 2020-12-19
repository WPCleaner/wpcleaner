/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a50x.a501;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.regex.Matcher;

import org.wikipediacleaner.api.check.Actionnable;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CompositeAction;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.Suggestion;
import org.wikipediacleaner.api.data.Suggestion.ElementarySuggestion;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsInterval;
import org.wikipediacleaner.api.data.contents.Interval;
import org.wikipediacleaner.api.data.analysis.Areas;
import org.wikipediacleaner.gui.swing.component.MWPaneReplaceAllAction;
import org.wikipediacleaner.i18n.GT;
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
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  @Override
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      boolean onlyAutomatic) {
    boolean result = false;
    if ((analysis == null) || (!analysis.shouldCheckSpelling())) {
      return result;
    }

    // Initialize active suggestions
    List<Suggestion> tmpSuggestions = onlyAutomatic ? automaticActiveSuggestions : allActiveSuggestions;
    if (tmpSuggestions.isEmpty()) {
      return false;
    }
    List<Suggestion> activeSuggestions = new ArrayList<>(tmpSuggestions);

    // Check spelling in templates
    List<Replacement> replacements = new ArrayList<>();
    if ((result == false) || (errors != null)) {
      result |= analyzeTemplates(analysis, activeSuggestions, replacements);
    }

    // Check spelling in internal links
    if ((result == false) || (errors != null)) {
      result |= analyzeInternalLinks(analysis, activeSuggestions, replacements);
    }

    // Check spelling in tags
    if ((result == false) || (errors != null)) {
      result |= analyzeTags(analysis, activeSuggestions, replacements);
    }

    // Check spelling in normal text with non native regular expressions
    if ((result == false) || (errors != null)) {
      result |= analyzeNonNativeText(analysis, activeSuggestions, replacements);
    }

    // Check spelling in normal text with native regular expressions
    if ((result == false) || (errors != null)) {
      result |= analyzeNativeText(analysis, activeSuggestions, replacements);
    }

    if (errors == null) {
      return result;
    }

    // Group replacements
    String contents = analysis.getContents();
    List<ReplacementGroup> groups = new ArrayList<>();
    ReplacementComparator comparator = new ReplacementComparator();
    Collections.sort(replacements);
    while (!replacements.isEmpty()) {
      List<Replacement> group = getFirstGroup(replacements);
      Collections.sort(group, comparator);
      groups.add(new ReplacementGroup(group, contents));
    }

    // Check for multiple replacements
    for (int numGroup1 = 0; numGroup1 < groups.size(); numGroup1++) {
      ReplacementGroup group1 = groups.get(numGroup1);
      List<Replacement> replacements1 = group1.getReplacements();
      int numGroup2 = numGroup1 + 1;
      for (numGroup2 = numGroup1 + 1; numGroup2 < groups.size(); numGroup2++) {
        ReplacementGroup group2 = groups.get(numGroup2);
        if (group1.getText().equals(group2.getText())) {
          List<Replacement> replacements2 = group2.getReplacements();
          for (Replacement replacement1 : replacements1) {
            if (!Boolean.TRUE.equals(replacement1.isMultiple())) {
              for (Replacement replacement2 : replacements2) {
                if (replacement1.getReplacement().equals(replacement2.getReplacement())) {
                  replacement1.setMultiple();
                  replacement2.setMultiple();
                }
              }
            }
          }
        }
      }
    }

    // Analyze replacements
    List<String> multiples = new ArrayList<>();
    for (ReplacementGroup group : groups) {

      // Create error
      CheckErrorResult error = createCheckErrorResult(
          analysis, group.getBegin(), group.getEnd());
      String previousComment = null;
      multiples.clear();
      for (Replacement replacement : group.getReplacements()) {

        // Manage comment
        String comment = replacement.getComment();
        if (comment == null) {
          error.addText(null);
        } else if (!comment.equals(previousComment)) {
          error.addText(comment);
        }
        previousComment = comment;

        error.addReplacement(replacement.getReplacement(), replacement.isAutomatic());
        if (Boolean.TRUE.equals(replacement.isMultiple())) {
          multiples.add(replacement.getReplacement());
        }
      }
      error.addReplacement(group.getText(), GT._T("Restore original text"));

      // Multiple replacements
      if (!multiples.isEmpty()) {
        if (multiples.size() == 1) {
          error.addPossibleAction(new SimpleAction(
              GT._T("Replace each time with {0}", multiples.get(0)),
              new MWPaneReplaceAllAction(group.getText(), multiples.get(0))));
        } else {
          List<Actionnable> actions = new ArrayList<>();
          for (String multiple : multiples) {
            actions.add(new SimpleAction(multiple, new MWPaneReplaceAllAction(group.getText(), multiple)));
          }
          error.addPossibleAction(new CompositeAction(GT._T("Replace each time with"), actions));
        }
      }
      errors.add(error);
    }

    return result;
  }

  /**
   * Check spelling in normal text with native regular expressions.
   * 
   * @param analysis Page analysis.
   * @param suggestions Active suggestions.
   * @param replacements List of possible replacements.
   * @return True if an error has been found.
   */
  private boolean analyzeNativeText(
      PageAnalysis analysis, List<Suggestion> suggestions,
      List<Replacement> replacements) {
    boolean result = false;

    // Check every suggestion
    List<Interval> chunks = computeContentsChunks(analysis, true);
    String contents = analysis.getContents();
    Iterator<Suggestion> itSuggestion = suggestions.iterator();
    List<Replacement> tmpReplacements = new ArrayList<>();
    while (itSuggestion.hasNext()) {
      Suggestion suggestion = itSuggestion.next();
      if (!suggestion.isOtherPattern()) {
        Performance perf = Performance.getInstance("Slow regular expression");
        perf.setThreshold(slowRegexp);
        itSuggestion.remove();
        Matcher matcher = suggestion.initMatcher(contents);
        for (Interval chunk : chunks) {
          matcher.region(chunk.getBeginIndex(), chunk.getEndIndex());
          int authorizedBegin = chunk.getBeginIndex();
          while (matcher.find()) {
            int begin = matcher.start();
            int end = matcher.end();
            boolean shouldKeep = true;
            if (shouldKeep && (begin > 0) &&
                (Character.isLetterOrDigit(contents.charAt(begin))) &&
                (Character.isLetterOrDigit(contents.charAt(begin - 1)))) {
              shouldKeep = false;
            }
            if (shouldKeep && (end < contents.length()) &&
                (Character.isLetterOrDigit(contents.charAt(end))) &&
                (Character.isLetterOrDigit(contents.charAt(end - 1)))) {
              shouldKeep = false;
            }
            if (shouldKeep) {
              tmpReplacements.clear();
              shouldKeep = addReplacements(
                  begin, end, contents, authorizedBegin, chunk.getEndIndex(),
                  suggestion, tmpReplacements);
            }
            if (shouldKeep && (analysis.getAreas().getEndArea(begin) > begin)) {
              shouldKeep = false;
            }
            if (shouldKeep && (analysis.isInTemplate(begin) != null)) {
              shouldKeep = false;
            }
            if (shouldKeep) {
              shouldKeep = shouldKeep(contents, begin, end);
            }
            if (shouldKeep) {
              result = true;
              replacements.addAll(tmpReplacements);
            }
            authorizedBegin = end;
          }
        }
        perf.printEnd(suggestion.getPatternText());
        perf.release();
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
   * @return True if an error has been found.
   */
  private boolean analyzeNonNativeText(
      PageAnalysis analysis, List<Suggestion> suggestions,
      List<Replacement> replacements) {
    boolean result = false;

    // Check every suggestion
    List<Interval> chunks = computeContentsChunks(analysis, false);
    String contents = analysis.getContents();
    Iterator<Suggestion> itSuggestion = suggestions.iterator();
    List<Replacement> tmpReplacements = new ArrayList<>();
    while (itSuggestion.hasNext()) {
      Suggestion suggestion = itSuggestion.next();
      if (suggestion.isOtherPattern()) {
        Performance perf = Performance.getInstance("Slow regular expression");
        perf.setThreshold(slowRegexp);
        itSuggestion.remove();
        Matcher matcher = suggestion.initMatcher(contents);
        for (Interval chunk : chunks) {
          matcher.region(chunk.getBeginIndex(), chunk.getEndIndex());
          int authorizedBegin = chunk.getBeginIndex();
          while (matcher.find()) {
            int begin = matcher.start();
            int end = matcher.end();
            tmpReplacements.clear();
            boolean shouldKeep = addReplacements(
                begin, end, contents, authorizedBegin, chunk.getEndIndex(),
                suggestion, tmpReplacements);
            if (shouldKeep) {
              shouldKeep = shouldKeep(contents, begin, end);
            }
            if (shouldKeep) {
              result = true;
              replacements.addAll(tmpReplacements);
            }
            authorizedBegin = end;
          }
        }
        perf.printEnd(suggestion.getComment(), suggestion.getPatternText());
        perf.release();
      }
    }

    return result;
  }

  /**
   * Decide if a potential spelling error should be kept as error.
   * 
   * @param contents Text.
   * @param begin Begin index of the potential spelling error.
   * @param end End index of the potential spelling error.
   * @return True if the potential error should be kept.
   */
  public boolean shouldKeep(String contents, int begin, int end) {
    boolean shouldKeep = true;

    // Check for potential URL
    if (shouldKeep) {
      int urlBegin = begin;
      while (urlBegin > 0) {
        char previousChar = contents.charAt(urlBegin - 1);
        if (!Character.isLetterOrDigit(previousChar) &&
            (".-".indexOf(previousChar) < 0)) {
          break;
        }
        urlBegin--;
      }
      int urlEnd = begin;
      int lastDot = -1;
      boolean onlyChars = true;
      while (urlEnd < contents.length()) {
        char currentChar = contents.charAt(urlEnd);
        if (!Character.isLetterOrDigit(currentChar) &&
            (".-".indexOf(currentChar) < 0)) {
          break;
        }
        if (!Character.isLetter(currentChar)) {
          onlyChars = false;
        }
        if (currentChar == '.') {
          lastDot = urlEnd;
          onlyChars = true;
        }
        urlEnd++;
      }
      //System.err.println(contents.substring(urlBegin, urlEnd));
      if ((urlEnd >= end) && (lastDot > 0) && onlyChars &&
          (urlEnd <= lastDot + 4) && (urlEnd > lastDot + 1)) {
        shouldKeep = false;
      }
    }
    
    return shouldKeep;
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

    // Check each suggestion
    List<PageElementTemplate> templates = analysis.getTemplates();
    List<PageElementFunction> functions = analysis.getFunctions();
    String contents = analysis.getContents();
    int contentsLength = contents.length();
    Iterator<Suggestion> itSuggestion = suggestions.iterator();
    while (itSuggestion.hasNext()) {
      Suggestion suggestion = itSuggestion.next();
      if (suggestion.getPatternText().startsWith("\\{\\{")) {
        itSuggestion.remove();
        Matcher matcher = suggestion.initMatcher(contents);

        // Check suggestion on each template
        for (PageElementTemplate template : templates) {
          int begin = template.getBeginIndex();
          if (matcher.region(begin, contentsLength).lookingAt()) {
            int end = matcher.end();
            if ((end >= contentsLength) ||
                (!Character.isLetterOrDigit(contents.charAt(end))) ||
                (!Character.isLetterOrDigit(contents.charAt(end - 1)))) {
              result |= addReplacements(
                  begin, end, contents, begin, contentsLength,
                  suggestion, replacements);
            }
          }
        }

        // Check suggestion on each function
        for (PageElementFunction function : functions) {
          int begin = function.getBeginIndex();
          if (matcher.region(begin, contentsLength).lookingAt()) {
            int end = matcher.end();
            if ((end >= contentsLength) ||
                (!Character.isLetterOrDigit(contents.charAt(end))) ||
                (!Character.isLetterOrDigit(contents.charAt(end - 1)))) {
              result |= addReplacements(
                  begin, end, contents, begin, contentsLength,
                  suggestion, replacements);
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

    // Check each suggestion
    List<PageElementInternalLink> links = analysis.getInternalLinks();
    String contents = analysis.getContents();
    int contentsLength = contents.length();
    Iterator<Suggestion> itSuggestion = suggestions.iterator();
    while (itSuggestion.hasNext()) {
      Suggestion suggestion = itSuggestion.next();
      if (suggestion.getPatternText().startsWith("\\[\\[")) {
        itSuggestion.remove();
        Matcher matcher = suggestion.initMatcher(contents);

        // Check suggestion on each internal link
        for (PageElementInternalLink link : links) {
          int begin = link.getBeginIndex();
          if (matcher.region(begin, contentsLength).lookingAt()) {
            int end = matcher.end();
            if ((end >= contentsLength) ||
                (!Character.isLetterOrDigit(contents.charAt(end))) ||
                (!Character.isLetterOrDigit(contents.charAt(end - 1)))) {
              result |= addReplacements(
                  begin, end, contents, begin, contentsLength,
                  suggestion, replacements);
            }
          }
        }
      }
    }

    return result;
  }

  /**
   * Check spelling in tags.
   * 
   * @param analysis Page analysis.
   * @param suggestions Active suggestions.
   * @param replacements List of possible replacements.
   * @return True if an error has been found.
   */
  private boolean analyzeTags(
      PageAnalysis analysis, List<Suggestion> suggestions,
      List<Replacement> replacements) {
    boolean result = false;

    // Check each suggestion
    List<PageElementTag> tags = analysis.getTags();
    String contents = analysis.getContents();
    int contentsLength = contents.length();
    Iterator<Suggestion> itSuggestion = suggestions.iterator();
    while (itSuggestion.hasNext()) {
      Suggestion suggestion = itSuggestion.next();
      if (suggestion.getPatternText().startsWith("<")) {
        itSuggestion.remove();
        Matcher matcher = suggestion.initMatcher(contents);

        // Check suggestion on each tag
        for (PageElementTag tag : tags) {
          int begin = tag.getBeginIndex();
          if (matcher.region(begin, contentsLength).lookingAt()) {
            int end = matcher.end();
            if ((end >= contentsLength) ||
                (!Character.isLetterOrDigit(contents.charAt(end))) ||
                (!Character.isLetterOrDigit(contents.charAt(end - 1)))) {
              result |= addReplacements(
                  begin, end, contents, begin, contentsLength,
                  suggestion, replacements);
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
      int authorizedBegin, int authorizedEnd,
      Suggestion suggestion, List<Replacement> replacements) {
    boolean result = false;
    String text = contents.substring(authorizedBegin, authorizedEnd);
    List<ElementarySuggestion> possibles = suggestion.getReplacements(
        text, begin - authorizedBegin, end - authorizedBegin);
    if (possibles != null) {
      text = contents.substring(begin, end);
      for (ElementarySuggestion element : possibles) {
        String possible = element.getReplacement();
        if (!text.equals(possible)) {
          Replacement replacement = new Replacement(
              begin, end,
              suggestion.getComment(), suggestion.isOtherPattern(),
              possible, element.isAutomatic());
          replacements.add(replacement);
          result = true;
        } else {
          // If a replacement gives the original text, it's a possible confusion
          return false;
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
    List<Replacement> firstGroup = new LinkedList<>();
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
   * Split contents into analyzable chunks.
   * 
   * @param analysis Page analysis.
   * @param nativeRegexp True if creating chunks for WPCleaner regular expressions.
   * @return List of contents chunks.
   */
  private List<Interval> computeContentsChunks(
      PageAnalysis analysis, boolean nativeRegexp) {
    String contents = analysis.getContents();
    List<Interval> chunks = new LinkedList<>();
    chunks.add(new ContentsInterval(0, contents.length()));

    // Remove templates
    if (!nativeRegexp) {
      List<PageElementTemplate> templates = analysis.getTemplates();
      for (PageElementTemplate template : templates) {
        removeArea(chunks, template.getBeginIndex(), template.getEndIndex());
      }
    }

    // Remove tags
    removeCompleteTags(chunks, analysis, PageElementTag.TAG_HTML_CODE);
    removeCompleteTags(chunks, analysis, PageElementTag.TAG_WIKI_GRAPH);
    removeCompleteTags(chunks, analysis, PageElementTag.TAG_WIKI_IMAGEMAP); // TODO: keep descriptions
    removeCompleteTags(chunks, analysis, PageElementTag.TAG_WIKI_MATH);
    removeCompleteTags(chunks, analysis, PageElementTag.TAG_WIKI_MATH_CHEM);
    removeCompleteTags(chunks, analysis, PageElementTag.TAG_WIKI_SCORE);
    removeCompleteTags(chunks, analysis, PageElementTag.TAG_WIKI_SOURCE);
    removeCompleteTags(chunks, analysis, PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT);
    removeCompleteTags(chunks, analysis, PageElementTag.TAG_WIKI_TIMELINE);
    removeGalleryTags(chunks, analysis);

    // Remove areas
    if (!nativeRegexp) {
      Areas areas = analysis.getAreas();
      for (Areas.Area area : areas.getAreas()) {
        removeArea(chunks, area.getBeginIndex(), area.getEndIndex());
      }
    }

    // Remove empty chunks
    Iterator<Interval> itChunks = chunks.iterator();
    while (itChunks.hasNext()) {
      Interval chunk = itChunks.next();
      int begin = chunk.getBeginIndex();
      int end = chunk.getEndIndex();
      String chunkContents = contents.substring(begin, end);
      int length = chunkContents.length();
      int currentIndex = 0;
      while ((currentIndex < length) &&
             (Character.isWhitespace(chunkContents.charAt(currentIndex)))) {
        currentIndex++;
      }
      if (currentIndex >= length) {
        itChunks.remove();
      }
    }

    return chunks;
  }

  /**
   * Remove complete tags from the list of chunks of text.
   * 
   * @param chunks List of chunks of text.
   * @param analysis Page analysis.
   * @param tagName Tag name to remove.
   */
  private void removeCompleteTags(List<Interval> chunks, PageAnalysis analysis, String tagName) {
    List<PageElementTag> tags = analysis.getCompleteTags(tagName);
    for (PageElementTag tag : tags) {
      removeArea(chunks, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
    }
  }

  /**
   * Remove gallery tags from the list of chunks of text.
   * 
   * @param chunks List of chunks of text.
   * @param analysis Page analysis.
   */
  private void removeGalleryTags(List<Interval> chunks, PageAnalysis analysis) {
    Namespace imageNamespace = analysis.getWikiConfiguration().getNamespace(Namespace.IMAGE);
    String contents = analysis.getContents();
    List<PageElementTag> tags = analysis.getCompleteTags(PageElementTag.TAG_WIKI_GALLERY);
    for (PageElementTag tag : tags) {
      removeArea(chunks, tag.getBeginIndex(), tag.getEndIndex());
      if (tag.isComplete() && !tag.isEndTag() && (tag.getMatchingTag() != null)) {
        PageElementTag endTag = tag.getMatchingTag();
        int beginIndex = tag.getEndIndex();
        int tmpIndex = beginIndex;
        while (tmpIndex <= endTag.getBeginIndex()) {
          if ((tmpIndex == endTag.getBeginIndex()) ||
              (contents.charAt(tmpIndex) == '\n')) {
            String line = contents.substring(beginIndex, tmpIndex).trim();
            int colonIndex = line.indexOf(':');
            if ((colonIndex > 0) && (imageNamespace.isPossibleName(line.substring(0, colonIndex)))) {
              int pipeIndex = line.indexOf('|', colonIndex);
              if (pipeIndex < 0) {
                removeArea(chunks, beginIndex, tmpIndex + 1);
              } else {
                removeArea(chunks, beginIndex, beginIndex + pipeIndex + 1);
              }
            } else {
              removeArea(chunks, beginIndex, tmpIndex + 1);
            }
            beginIndex = tmpIndex + 1;
          }
          tmpIndex++;
        }
        removeArea(chunks, endTag.getBeginIndex(), endTag.getEndIndex());
      }
    }
  }

  /**
   * Remove an area from the list of chunks of text.
   * 
   * @param chunks List of chunks of text.
   * @param begin Begin of the area to remove.
   * @param end End of the area to remove.
   */
  private void removeArea(List<Interval> chunks, int begin, int end) {
    ListIterator<Interval> itChunks = chunks.listIterator();
    while (itChunks.hasNext()) {
      Interval chunk = itChunks.next();
      if ((begin >= chunk.getEndIndex()) ||
          (end <= chunk.getBeginIndex())) {
        // Nothing to do
      } else {
        itChunks.remove();
        if (begin > chunk.getBeginIndex()) {
          itChunks.add(new ContentsInterval(chunk.getBeginIndex(), begin));
        }
        if (end < chunk.getEndIndex()) {
          itChunks.add(new ContentsInterval(end, chunk.getEndIndex()));
        }
      }
    }
  }

  /**
   * Automatic fixing of some errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    // Initialize active suggestions
    allActiveSuggestions.clear();
    automaticActiveSuggestions.clear();
    Map<String, Suggestion> suggestions = getWPCConfiguration().getSuggestions();
    if (suggestions != null) {
      for (Suggestion suggestion : suggestions.values()) {
        if (suggestion.isActive()) {
          allActiveSuggestions.add(suggestion);
          if (suggestion.hasAutomaticReplacements()) {
            automaticActiveSuggestions.add(suggestion);
          }
        }
      }
    }

    // Initialize limit for slow regular expressions
    Configuration config = Configuration.getConfiguration();
    slowRegexp = config.getInt(null, ConfigurationValueInteger.SLOW_REGEXP);
  }

  /** Active suggestions */
  private final List<Suggestion> allActiveSuggestions = new LinkedList<>();

  /** Active suggestions with automatic replacements */
  private final List<Suggestion> automaticActiveSuggestions = new LinkedList<>();

  /** Limit for reporting a regular expression as being slow */
  private int slowRegexp = 1000;
}
