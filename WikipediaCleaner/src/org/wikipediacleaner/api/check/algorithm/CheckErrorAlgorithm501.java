/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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

import org.wikipediacleaner.api.check.Actionnable;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CompositeAction;
import org.wikipediacleaner.api.check.NullActionProvider;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementAreas;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.Suggestion;
import org.wikipediacleaner.api.data.Suggestion.ElementarySuggestion;
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
    Map<String, Suggestion> suggestions = analysis.getWPCConfiguration().getSuggestions();
    if ((suggestions == null) || (suggestions.isEmpty())) {
      return result;
    }
    List<Suggestion> activeSuggestions = new LinkedList<Suggestion>();
    for (Suggestion suggestion : suggestions.values()) {
      if (suggestion.isActive()) {
        if (!onlyAutomatic || suggestion.hasAutomaticReplacements()) {
          activeSuggestions.add(suggestion);
        }
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
      result |= analyzeNonNativeText(analysis, activeSuggestions, replacements, slowRegexp);
    }

    // Check spelling in normal text with native regular expressions
    if ((result == false) || (errors != null)) {
      result |= analyzeNativeText(analysis, activeSuggestions, replacements, slowRegexp);
    }

    if (errors == null) {
      return result;
    }

    // Group replacements
    String contents = analysis.getContents();
    List<ReplacementGroup> groups = new ArrayList<ReplacementGroup>();
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
    List<String> multiples = new ArrayList<String>();
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
          error.addPossibleAction(null, new NullActionProvider());
        } else if (!comment.equals(previousComment)) {
          error.addPossibleAction(comment, new NullActionProvider());
        }
        previousComment = comment;

        error.addReplacement(replacement.getReplacement(), replacement.isAutomatic());
        if (Boolean.TRUE.equals(replacement.isMultiple())) {
          multiples.add(replacement.getReplacement());
        }
      }
      error.addReplacement(group.getText(), GT._("Restore original text"));

      // Multiple replacements
      if (!multiples.isEmpty()) {
        if (multiples.size() == 1) {
          error.addPossibleAction(new SimpleAction(
              GT._("Replace each time with {0}", multiples.get(0)),
              new MWPaneReplaceAllAction(group.getText(), multiples.get(0))));
        } else {
          List<Actionnable> actions = new ArrayList<Actionnable>();
          for (String multiple : multiples) {
            actions.add(new SimpleAction(multiple, new MWPaneReplaceAllAction(group.getText(), multiple)));
          }
          error.addPossibleAction(new CompositeAction(GT._("Replace each time with"), actions));
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
   * @param slowRegexp Threshold for slow regular expression.
   * @return True if an error has been found.
   */
  private boolean analyzeNativeText(
      PageAnalysis analysis, List<Suggestion> suggestions,
      List<Replacement> replacements, int slowRegexp) {
    boolean result = false;

    // Check every suggestion
    List<ContentsChunk> chunks = computeContentsChunks(analysis, true);
    String contents = analysis.getContents();
    Iterator<Suggestion> itSuggestion = suggestions.iterator();
    List<Replacement> tmpReplacements = new ArrayList<CheckErrorAlgorithm501.Replacement>();
    while (itSuggestion.hasNext()) {
      Suggestion suggestion = itSuggestion.next();
      if (!suggestion.isOtherPattern()) {
        Performance perf = new Performance("Slow regular expression");
        perf.setThreshold(slowRegexp);
        itSuggestion.remove();
        Matcher matcher = suggestion.initMatcher(contents);
        for (ContentsChunk chunk : chunks) {
          matcher.region(chunk.getBegin(), chunk.getEnd());
          int authorizedBegin = chunk.getBegin();
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
                  begin, end, contents, authorizedBegin, chunk.getEnd(),
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
    List<ContentsChunk> chunks = computeContentsChunks(analysis, false);
    String contents = analysis.getContents();
    Iterator<Suggestion> itSuggestion = suggestions.iterator();
    List<Replacement> tmpReplacements = new ArrayList<CheckErrorAlgorithm501.Replacement>();
    while (itSuggestion.hasNext()) {
      Suggestion suggestion = itSuggestion.next();
      if (suggestion.isOtherPattern()) {
        Performance perf = new Performance("Slow regular expression");
        perf.setThreshold(slowRegexp);
        itSuggestion.remove();
        Matcher matcher = suggestion.initMatcher(contents);
        for (ContentsChunk chunk : chunks) {
          matcher.region(chunk.getBegin(), chunk.getEnd());
          int authorizedBegin = chunk.getBegin();
          while (matcher.find()) {
            int begin = matcher.start();
            int end = matcher.end();
            tmpReplacements.clear();
            boolean shouldKeep = addReplacements(
                begin, end, contents, authorizedBegin, chunk.getEnd(),
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
   * Split contents into analyzable chunks.
   * 
   * @param analysis Page analysis.
   * @param nativeRegexp True if creating chunks for WPCleaner regular expressions.
   * @return List of contents chunks.
   */
  private List<ContentsChunk> computeContentsChunks(
      PageAnalysis analysis, boolean nativeRegexp) {
    String contents = analysis.getContents();
    List<ContentsChunk> chunks = new LinkedList<ContentsChunk>();
    chunks.add(new ContentsChunk(0, contents.length()));

    // Remove templates
    if (!nativeRegexp) {
      List<PageElementTemplate> templates = analysis.getTemplates();
      for (PageElementTemplate template : templates) {
        removeArea(chunks, template.getBeginIndex(), template.getEndIndex());
      }
    }

    // Remove tags
    removeCompleteTags(chunks, analysis, PageElementTag.TAG_WIKI_CODE);
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
      PageElementAreas areas = analysis.getAreas();
      for (PageElementAreas.Area area : areas.getAreas()) {
        removeArea(chunks, area.getBeginIndex(), area.getEndIndex());
      }
    }

    // Remove empty chunks
    Iterator<ContentsChunk> itChunks = chunks.iterator();
    while (itChunks.hasNext()) {
      ContentsChunk chunk = itChunks.next();
      int begin = chunk.getBegin();
      int end = chunk.getEnd();
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
  private void removeCompleteTags(List<ContentsChunk> chunks, PageAnalysis analysis, String tagName) {
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
  private void removeGalleryTags(List<ContentsChunk> chunks, PageAnalysis analysis) {
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
  private void removeArea(List<ContentsChunk> chunks, int begin, int end) {
    ListIterator<ContentsChunk> itChunks = chunks.listIterator();
    while (itChunks.hasNext()) {
      ContentsChunk chunk = itChunks.next();
      if ((begin >= chunk.getEnd()) || (end <= chunk.getBegin())) {
        // Nothing to do
      } else {
        itChunks.remove();
        if (begin > chunk.getBegin()) {
          itChunks.add(new ContentsChunk(chunk.getBegin(), begin));
        }
        if (end < chunk.getEnd()) {
          itChunks.add(new ContentsChunk(end, chunk.getEnd()));
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
    return fixUsingAutomaticReplacement(analysis);
  }

  /**
   * Utility class to manage chunks of text.
   */
  private static class ContentsChunk {
    private final int begin;
    private final int end;

    public ContentsChunk(int begin, int end) {
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
    private final boolean automatic;
    private Boolean multiple;

    public Replacement(
        int begin, int end,
        String comment, boolean otherPattern,
        String replacement, boolean automatic) {
      this.begin = begin;
      this.end = end;
      this.otherPattern = otherPattern;
      this.comment = comment;
      this.replacement = replacement;
      this.automatic = automatic;
      this.multiple = null;
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

    public boolean isAutomatic() {
      return automatic;
    }

    public Boolean isMultiple() {
      return multiple;
    }

    public void setMultiple() {
      multiple = Boolean.TRUE;
    }
    /**
     * @param o
     * @return
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
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
    @Override
    public int compare(Replacement o1, Replacement o2) {
      
      // Comparison on native pattern
      if (o1.isOtherPattern() != o2.isOtherPattern()) {
        return (o1.isOtherPattern() ? 1 : -1);
      }

      // Comparison on automatic
      if (o1.isAutomatic() != o2.isAutomatic()) {
        return o1.isAutomatic() ? -1 : 1;
      }

      // Comparison on begin
      if (o1.getBegin() != o2.getBegin()) {
        return (o1.getBegin() < o2.getBegin() ? -1 : 1);
      }

      // Comparison on end
      if (o1.getEnd() != o2.getEnd()) {
        return (o1.getEnd() > o2.getEnd() ? -1 : 1);
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

      return 0;
    }
  }

  /**
   * Utility class to group possible replacements.
   */
  private static class ReplacementGroup {

    /**
     * Minimum index for the group of replacements.
     */
    private final int begin;

    /**
     * Maximum index for the group of replacements.
     */
    private final int end;

    /**
     * Original text.
     */
    private final String text;

    /**
     * Group of replacements.
     */
    private final List<Replacement> group;

    /**
     * Create a group of replacements.
     * 
     * @param replacements Ordered list of replacements.
     * @param contents Page contents.
     */
    public ReplacementGroup(List<Replacement> replacements, String contents) {

      // Compute minimum/maximum indexes for the group of replacements.
      int minBegin = Integer.MAX_VALUE;
      int maxEnd = 0;
      for (Replacement replacement : replacements) {
        minBegin = Math.min(minBegin, replacement.getBegin());
        maxEnd = Math.max(maxEnd, replacement.getEnd());
      }
      begin = minBegin;
      end = maxEnd;
      text = contents.substring(begin, end);

      // Compute new replacements using the full expand of the group.
      List<String> alreadyAdded = new ArrayList<String>();
      group = new ArrayList<Replacement>();
      for (Replacement replacement : replacements) {
        if ((replacement.getBegin() == begin) &&
            (replacement.getEnd() == end)) {
          String newText = replacement.getReplacement();
          if (!alreadyAdded.contains(newText)) {
            group.add(replacement);
            alreadyAdded.add(newText);
          }
        } else {
          String newText =
              contents.substring(begin, replacement.getBegin()) +
              replacement.getReplacement() +
              contents.substring(replacement.getEnd(), end);
          if (!alreadyAdded.contains(newText)) {
            group.add(
                new Replacement(
                    begin, end, replacement.getComment(),replacement.isOtherPattern(),
                    newText, replacement.isAutomatic()));
            alreadyAdded.add(newText);
          }
        }
      }
    }

    /**
     * @return Minimum index for the group of replacements.
     */
    public int getBegin() {
      return begin;
    }

    /**
     * @return Maximum index for the group of replacements.
     */
    public int getEnd() {
      return end;
    }

    /**
     * @return Original text.
     */
    public String getText() {
      return text;
    }

    /**
     * @return List of possible replacements.
     */
    public List<Replacement> getReplacements() {
      return group;
    }
  }
}
