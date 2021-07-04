/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a06x.a069;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmISBN;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElement;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementInterwikiLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.api.data.contents.template.TemplateBuilder;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 69 of check wikipedia project.
 * Error 69: ISBN wrong syntax
 */
public class CheckErrorAlgorithm069 extends CheckErrorAlgorithmISBN {

  public CheckErrorAlgorithm069() {
    super("ISBN wrong syntax");
  }

  /** List of strings that could be before an ISBN in <nowiki>. */
  private final static String[] EXTEND_BEFORE_NOWIKI = {
    WikiTagType.NOWIKI.getOpenTag(),
    HtmlTagType.SMALL.getOpenTag(),
    "(",
  };

  /** List of strings that could be after an ISBN in <nowiki>. */
  private final static String[] EXTEND_AFTER_NOWIKI = {
    WikiTagType.NOWIKI.getCloseTag(),
    HtmlTagType.SMALL.getCloseTag(),
    ")",
  };

  /** List of strings that could be between "ISBN" and its value. */
  private final static String[] FIRST_SEPARATOR = {
    "&nbsp;",
    "&#x20;",
  };

  private final static Map<String, String> PREFIX_INTERNAL_LINK = new HashMap<>();

  private static final String SMALL_CLOSE = HtmlTagType.SMALL.getCloseTag();
  private static final String SMALL_OPEN = HtmlTagType.SMALL.getOpenTag();

  /** Names of special page BookSources depending on the wiki */
  private final static Map<String, Pair<Set<String>, Set<String>>> BOOK_SOURCES = new HashMap<>();

  static {
    // Configure PREFIX_INTERNAL_LINK
    PREFIX_INTERNAL_LINK.put(PageElementISBN.ISBN_PREFIX, "");
    PREFIX_INTERNAL_LINK.put("(" + PageElementISBN.ISBN_PREFIX, "(");

    // Configure BOOK_SOURCES
    Pair<Set<String>, Set<String>> wiki = null;
    Set<String> namespaceNames = null;
    Set<String> pageNames = null;

    // CS
    namespaceNames = new HashSet<>();
    namespaceNames.add("Speciální");
    pageNames = new HashSet<>();
    pageNames.add("BookSources");
    pageNames.add("Zdroje knih");
    pageNames.add("KnižnéZdroje");
    wiki = new ImmutablePair<>(namespaceNames, pageNames);
    BOOK_SOURCES.put("cs", wiki);

    // DE
    namespaceNames = new HashSet<>();
    namespaceNames.add("Spezial");
    pageNames = new HashSet<>();
    pageNames.add("BookSources");
    pageNames.add("ISBN Suche");
    pageNames.add("ISBN-Suche");
    wiki = new ImmutablePair<>(namespaceNames, pageNames);
    BOOK_SOURCES.put("de", wiki);

    // EN
    namespaceNames = new HashSet<>();
    namespaceNames.add("Special");
    pageNames = new HashSet<>();
    pageNames.add("BookSources");
    wiki = new ImmutablePair<>(namespaceNames, pageNames);
    BOOK_SOURCES.put("en", wiki);

    // FR
    namespaceNames = new HashSet<>();
    namespaceNames.add("Spécial");
    namespaceNames.add("Sp%C3%A9cial");
    pageNames = new HashSet<>();
    pageNames.add("BookSources");
    pageNames.add("Ouvrages de référence");
    pageNames.add("Ouvrages de reference");
    pageNames.add("Ouvragesderéférence");
    pageNames.add("Ouvragesdereference");
    pageNames.add("Recherche ISBN");
    pageNames.add("Recherche isbn");
    pageNames.add("RechercheISBN");
    pageNames.add("Rechercheisbn");
    pageNames.add("Ouvrages%20de%20r%C3%A9f%C3%A9rence");
    wiki = new ImmutablePair<>(namespaceNames, pageNames);
    BOOK_SOURCES.put("fr", wiki);

    // IT
    namespaceNames = new HashSet<>();
    namespaceNames.add("Speciale");
    pageNames = new HashSet<>();
    pageNames.add("BookSources");
    pageNames.add("RicercaISBN");
    wiki = new ImmutablePair<>(namespaceNames, pageNames);
    BOOK_SOURCES.put("it", wiki);

    // NL
    namespaceNames = new HashSet<>();
    namespaceNames.add("Speciaal");
    pageNames = new HashSet<>();
    pageNames.add("Boekbronnen");
    pageNames.add("Boekinformatie");
    pageNames.add("BookSources");
    wiki = new ImmutablePair<>(namespaceNames, pageNames);
    BOOK_SOURCES.put("nl", wiki);
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
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Analyze each ISBN
    boolean result = false;
    result |= analyzeISBN(analysis, errors);

    // Report also ISBN like [[International Standard Book Number|ISBN]]&nbsp;978-0321637734
    result |= analyzeInternalLinks(analysis, errors);

    // Report also ISBN inside <nowiki> tags
    result |= analyzeNowikiTags(analysis, errors);

    // Report also ISBN in interwiki links
    result |= analyzeInterwikiLinks(analysis, errors);

    return result;
  }

  /**
   * Analyze ISBN to check if an ISBN error is present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeISBN(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {

    boolean result = false;
    List<PageElementISBN> isbns = analysis.getISBNs();
    for (PageElementISBN isbn : isbns) {
      boolean isError = false;
      if (!isbn.isCorrect() && isbn.isValid()) {
        isError = true;
      }

      // Exclude special configured values for ISBN
      if (isError && isbn.isTemplateParameter()) {
        WPCConfiguration config = analysis.getWPCConfiguration();
        List<String[]> specialValues = config.getStringArrayList(
            WPCConfigurationStringList.ISBN_SPECIAL_VALUES);
        if ((specialValues != null) && !specialValues.isEmpty()) {
          PageElementTemplate template = analysis.isInTemplate(isbn.getBeginIndex());
          if (template != null) {
            Parameter param = template.getParameterAtIndex(isbn.getBeginIndex());
            if ((param != null) &&
                (param.getName() != null) &&
                (param.getName().trim().length() > 0)) {
              String name = param.getName().trim();
              for (String[] specialValue : specialValues) {
                if ((specialValue.length > 2) &&
                    (Page.areSameTitle(template.getTemplateName(), specialValue[0])) &&
                    (name.equals(specialValue[1])) &&
                    (isbn.getISBNNotTrimmed().equals(specialValue[2]))) {
                  isError = false;
                }
              }
            }
          }
        }
      }

      // Exclude parameters in templates
      if (isError &&
          isbn.isTemplateParameter() &&
          analysis.isInNamespace(Namespace.TEMPLATE)) {
        PageElementTemplate template = analysis.isInTemplate(isbn.getBeginIndex());
        if (template != null) {
          Parameter param = template.getParameterAtIndex(isbn.getBeginIndex());
          if (param != null) {
            List<PageElementFunction> functions = analysis.getFunctions();
            if (functions != null) {
              for (PageElementFunction function : functions) {
                int functionIndex = function.getBeginIndex();
                if ((template == analysis.isInTemplate(functionIndex)) &&
                    (param == template.getParameterAtIndex(functionIndex))) {
                  isError = false;
                }
              }
            }
          }
        }
      }

      // Report error
      boolean reported = false;
      if (isError) {
        if (errors == null) {
          return true;
        }
        result = true;
        reported = true;

        // Check for potential extra characters around
        int beginIndex = isbn.getBeginIndex();
        int endIndex = isbn.getEndIndex();
        String contents = analysis.getContents();
        boolean tryAgain = true;
        while (tryAgain) {
          tryAgain = false;
          if ((beginIndex > 0) && (endIndex < contents.length())) {
            char previousChar = contents.charAt(beginIndex - 1);
            char nextChar = contents.charAt(endIndex);
            if (((previousChar == '(') && (nextChar == ')')) ||
                ((previousChar == '[') && (nextChar == ']'))) {
              beginIndex--;
              endIndex++;
              tryAgain = true;
            }
          }
        }
        if ((beginIndex >= SMALL_OPEN.length()) && (endIndex < contents.length())) {
          if (contents.startsWith(SMALL_OPEN, beginIndex - SMALL_OPEN.length()) &&
              contents.startsWith(SMALL_CLOSE, endIndex)) {
            beginIndex -= SMALL_OPEN.length();
            endIndex += SMALL_CLOSE.length();
          }
        }

        CheckErrorResult errorResult = createCheckErrorResult(analysis, isbn, false);
        String prefix = null;
        String suffix = null;
        if ((beginIndex < isbn.getBeginIndex()) && (endIndex > isbn.getEndIndex())) {
          prefix = contents.substring(beginIndex, isbn.getBeginIndex());
          suffix = contents.substring(isbn.getEndIndex(), endIndex);
          errorResult = createCheckErrorResult(
              analysis, beginIndex, endIndex, errorResult.getErrorLevel());
        }
        addSuggestions(analysis, errorResult, isbn);
        errors.add(errorResult);
        List<String> replacements = isbn.getCorrectISBN();
        if (replacements != null) {
          for (String replacement : replacements) {
            if (!replacement.equals(analysis.getContents().substring(isbn.getBeginIndex(), isbn.getEndIndex()))) {
              if ((prefix != null) && (suffix != null)) {
                errorResult.addReplacement(prefix + replacement + suffix);
              }
              errorResult.addReplacement(replacement);
            }
          }
        }
      }

      // Analyze to find links to Special/BookSources
      if (!reported && !isbn.isTemplateParameter()) {
        PageElement element = null;
        ErrorLevel level = ErrorLevel.CORRECT;
        String isbnText = analysis.getContents().substring(isbn.getBeginIndex(), isbn.getEndIndex());
        PageElementInternalLink link = analysis.isInInternalLink(isbn.getBeginIndex());
        if ((link != null) && (isbnText.equals(link.getText()))) {
          level = isSpecialBookSources(analysis, link.getLink());
          if (level != ErrorLevel.CORRECT) {
            element = link;
          }
        }
        if (element == null) {
          PageElementInterwikiLink iwLink = analysis.isInInterwikiLink(isbn.getBeginIndex());
          if ((iwLink != null) && (isbnText.equals(iwLink.getText()))) {
            level = isSpecialBookSources(analysis, iwLink.getLink());
            if (level != ErrorLevel.CORRECT) {
              element = iwLink;
            }
          }
        }
        if (element != null) {
          if (errors == null) {
            return true;
          }
          result = true;
          reported = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, element.getBeginIndex(), element.getEndIndex(), level);
          List<String> replacements = isbn.getCorrectISBN();
          for (String replacement : replacements) {
            errorResult.addReplacement(replacement);
          }
          errors.add(errorResult);
        }
      }

      // Analyze if ISBN is inside an external link
      if (!reported && !isbn.isTemplateParameter()) {
        PageElementExternalLink link = analysis.isInExternalLink(isbn.getBeginIndex());
        if ((link != null) && link.hasSquare() &&
            (isbn.getBeginIndex() >= link.getBeginIndex() + link.getTextOffset()) &&
            (link.getText() != null)) {
          if (errors == null) {
            return true;
          }
          result = true;
          reported = true;
          
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, link.getBeginIndex(), link.getEndIndex());
          int beginIndex = isbn.getBeginIndex();
          int realEndIndex = isbn.getEndIndex();
          String contents = analysis.getContents();
          while ((beginIndex > 0) &&
                 (" ,;.(".indexOf(contents.charAt(beginIndex - 1)) >= 0)) {
            beginIndex--;
          }
          if (realEndIndex < link.getEndIndex()) {
            int tmpIndex = realEndIndex;
            while ((tmpIndex < link.getEndIndex()) &&
                   (", ".indexOf(contents.charAt(tmpIndex)) >= 0)) {
              tmpIndex++;
            }
            if ((tmpIndex < link.getEndIndex()) &&
                (contents.startsWith(isbn.getISBN(), tmpIndex))) {
              realEndIndex = tmpIndex + isbn.getISBN().length();
            }
          }
          int endIndex = realEndIndex;
          while ((endIndex < link.getEndIndex()) &&
                 (")".indexOf(contents.charAt(endIndex)) >= 0)) {
            endIndex++;
          }
          if (beginIndex > link.getBeginIndex() + link.getTextOffset()) {
            String replacementPrefix =
                contents.substring(link.getBeginIndex(), beginIndex) +
                contents.substring(endIndex, link.getEndIndex()) +
                contents.substring(beginIndex, isbn.getBeginIndex());
            String textPrefix =
                contents.substring(link.getBeginIndex(), link.getBeginIndex() + 7) +
                "...]" +
                contents.substring(beginIndex, isbn.getBeginIndex());
            List<String> replacements = isbn.getCorrectISBN();
            for (String replacement : replacements) {
              errorResult.addReplacement(
                  replacementPrefix + replacement + contents.substring(realEndIndex, endIndex),
                  textPrefix + replacement + contents.substring(realEndIndex, endIndex));
            }
            errorResult.addReplacement(
                replacementPrefix + contents.substring(isbn.getBeginIndex(), isbn.getEndIndex()),
                textPrefix + contents.substring(isbn.getBeginIndex(), isbn.getEndIndex()));
            if (endIndex < link.getEndIndex()) {
              replacementPrefix =
                  contents.substring(link.getBeginIndex(), beginIndex) +
                  "]" +
                  contents.substring(beginIndex, isbn.getBeginIndex());
              for (String replacement : replacements) {
                errorResult.addReplacement(
                    replacementPrefix + replacement + contents.substring(isbn.getEndIndex(), link.getEndIndex() - 1),
                    textPrefix + replacement + contents.substring(isbn.getEndIndex(), link.getEndIndex() - 1));
              }
              errorResult.addReplacement(
                  replacementPrefix + contents.substring(isbn.getBeginIndex(), link.getEndIndex() - 1),
                  textPrefix + contents.substring(isbn.getBeginIndex(), link.getEndIndex() - 1));
            }
          } else if (endIndex >= link.getEndIndex() - 1) {
            List<String> replacements = isbn.getCorrectISBN();
            for (String replacement : replacements) {
              errorResult.addReplacement(replacement);
            }
            errorResult.addReplacement(contents.substring(isbn.getBeginIndex(), isbn.getEndIndex()));
          }
          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /**
   * Analyze internal links to check if an ISBN error is present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeInternalLinks(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {

    List<PageElementInternalLink> links = analysis.getInternalLinks();
    if (links == null) {
      return false;
    }

    boolean result = false;
    for (PageElementInternalLink link : links) {
      if (analyzeInternalLinkPrefix(analysis, errors, link)) {
        result = true;
      } else {
        result |= analyzeInternalLinkInterwiki(analysis, errors, link);
      }
    }

    return result;
  }

  /**
   * Analyze an internal link to check if an ISBN error is present due to ISBN prefix.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param link Internal link to be checked.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeInternalLinkPrefix(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementInternalLink link) {

    // Check for the presence of the ISBN prefix
    String extraPrefix = PREFIX_INTERNAL_LINK.get(link.getDisplayedText().trim());

    // Move to the beginning of the potential ISBN value
    int tmpIndex = link.getEndIndex();
    String contents = analysis.getContents();
    boolean shouldContinue = true;
    while (shouldContinue) {
      shouldContinue = false;
      if (tmpIndex < contents.length()) {
        if (" \u00A0".indexOf(contents.charAt(tmpIndex)) >= 0) {
          tmpIndex++;
          shouldContinue = true;
        } else {
          for (String separator : FIRST_SEPARATOR) {
            if (contents.startsWith(separator, tmpIndex)) {
              tmpIndex += separator.length();
              shouldContinue = true;
            }
          }
        }
      }
    }

    // Analyze if there's an ISBN value
    boolean isbnFound = false;
    int beginISBN = tmpIndex;
    String suffix = null;
    if (tmpIndex < contents.length()) {
      PageElementInternalLink nextLink = null;
      PageElementExternalLink nextLinkE = null;
      if (contents.charAt(tmpIndex) == '[') {
        nextLink = analysis.isInInternalLink(tmpIndex);
        if (nextLink != null) {
          int offset = nextLink.getTextOffset();
          if (offset > 0) {
            tmpIndex += offset;
          } else {
            tmpIndex += 2;
          }
        } else {
          nextLinkE = analysis.isInExternalLink(tmpIndex);
          if (nextLinkE != null) {
            int offset = nextLinkE.getTextOffset();
            if (offset > 0) {
              tmpIndex += offset;
            } else {
              tmpIndex += 1;
            }
          }
        }
      }
      boolean endFound = false;
      while (!endFound) {
        endFound = true;
        if ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == '<')) {
          PageElementTag tag = analysis.isInTag(tmpIndex);
          if ((tag != null) && (tag.getBeginIndex() == tmpIndex)) {
            tmpIndex = tag.getEndIndex();
            endFound = false;
          }
        }
      }
      if ((tmpIndex < contents.length()) &&
          (PageElementISBN.POSSIBLE_CHARACTERS.indexOf(contents.charAt(tmpIndex)) >= 0)) {
        isbnFound = true;
      }
      if (nextLink != null) {
        suffix = nextLink.getDisplayedText();
        tmpIndex = nextLink.getEndIndex();
      } else if (nextLinkE != null) {
        suffix = nextLinkE.getDisplayedText();
        tmpIndex = nextLinkE.getEndIndex();
      } else {
        while ((tmpIndex < contents.length()) &&
               ((PageElementISBN.POSSIBLE_CHARACTERS.indexOf(contents.charAt(tmpIndex)) >= 0) ||
                (PageElementISBN.EXTRA_CHARACTERS.indexOf(contents.charAt(tmpIndex)) >= 0 ))) {
          tmpIndex++;
        }
        suffix = contents.substring(beginISBN, tmpIndex);
      }
    }
    if (!isbnFound) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, link.getBeginIndex(), tmpIndex);
    errorResult.addReplacement(
        extraPrefix + PageElementISBN.ISBN_PREFIX + " " + suffix);
    errors.add(errorResult);
    return true;
  }

  /**
   * Analyze an internal link to check if an ISBN error is present due to similarity with an interwiki link.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param link Internal link to be checked.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeInternalLinkInterwiki(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementInternalLink link) {

    // Check that the target of the links contains a name space separator.
    String target = link.getLink();
    int colonIndex = target.indexOf(':');
    if (colonIndex < 0) {
      return false;
    }

    // Check each pair Special/Book sources to see if it matches
    String prefix = target.substring(0, colonIndex);
    String suffix = target.substring(colonIndex + 1);
    int slashIndex = suffix.indexOf('/');
    String suffix2 = (slashIndex > 0) ? suffix.substring(0, slashIndex) : suffix;
    for (Pair<Set<String>, Set<String>> bookSource : BOOK_SOURCES.values()) {
      boolean prefixFound = false;
      for (String possiblePrefix : bookSource.getLeft()) {
        prefixFound |= Page.areSameTitle(prefix, possiblePrefix);
      }
      if (prefixFound) {
        for (String possibleSuffix : bookSource.getRight()) {
          if (Page.areSameTitle(suffix, possibleSuffix) ||
              Page.areSameTitle(suffix2, possibleSuffix)) {
            if (errors == null) {
              return true;
            }
            CheckErrorResult errorResult  = createCheckErrorResult(analysis, link.getBeginIndex(), link.getEndIndex());
            errorResult.addReplacement(link.getDisplayedText());
            errors.add(errorResult);
            return true;
          }
        }
      }
    }

    return false;
  }

  /**
   * Analyze nowiki tags to check if an ISBN is present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeNowikiTags(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {

    List<PageElementTag> nowikiTags = analysis.getCompleteTags(WikiTagType.NOWIKI);
    if (nowikiTags == null) {
      return false;
    }

    boolean result = false;
    String contents = analysis.getContents();
    for (PageElementTag nowikiTag : nowikiTags) {
      if (!nowikiTag.isFullTag() && nowikiTag.isComplete()) {
        String nowikiContent = contents.substring(
            nowikiTag.getValueBeginIndex(), nowikiTag.getValueEndIndex());
        int index = 0;
        while (index < nowikiContent.length()) {
          if (nowikiContent.startsWith(PageElementISBN.ISBN_PREFIX, index)) {
            int tmpIndex = index + PageElementISBN.ISBN_PREFIX.length();
            boolean hasSeparator = false;
            while ((tmpIndex < nowikiContent.length()) && 
                   (PageElementISBN.EXTRA_CHARACTERS.indexOf(nowikiContent.charAt(tmpIndex)) >= 0)) {
              hasSeparator = true;
              tmpIndex++;
            }
            boolean hasCharacter = false;
            int indexCharacter = tmpIndex;
            boolean shouldContinue = true;
            while (shouldContinue) {
              int tmpIndex2 = tmpIndex;
              shouldContinue = false;
              while ((tmpIndex2 < nowikiContent.length()) &&
                     (PageElementISBN.EXTRA_CHARACTERS.indexOf(nowikiContent.charAt(tmpIndex2)) >= 0)) {
                tmpIndex2++;
              }
              while ((tmpIndex2 < nowikiContent.length()) &&
                     (PageElementISBN.POSSIBLE_CHARACTERS.indexOf(nowikiContent.charAt(tmpIndex2)) >= 0)) {
                hasCharacter = true;
                shouldContinue = true;
                tmpIndex2++;
              }
              if (shouldContinue) {
                tmpIndex = tmpIndex2;
              }
            }
            if (hasSeparator && hasCharacter) {
              if (errors == null) {
                return true;
              }
              result = true;

              // Try to extend area
              int beginIndex = nowikiTag.getValueBeginIndex() + index;
              boolean extensionFound = false;
              do {
                extensionFound = false;
                for (String before : EXTEND_BEFORE_NOWIKI) {
                  if ((beginIndex >= before.length()) &&
                      (contents.startsWith(before, beginIndex - before.length()))) {
                    extensionFound = true;
                    beginIndex -= before.length();
                  }
                }
              } while (extensionFound);
              int endIndex = nowikiTag.getValueBeginIndex() + tmpIndex;
              do {
                extensionFound = false;
                for (String after : EXTEND_AFTER_NOWIKI) {
                  if ((endIndex < contents.length()) &&
                      (contents.startsWith(after, endIndex))) {
                    extensionFound = true;
                    endIndex += after.length();
                  }
                }
              } while (extensionFound);

              // Report error
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, beginIndex, endIndex);
              if ((beginIndex <= nowikiTag.getCompleteBeginIndex()) &&
                  (endIndex >= nowikiTag.getCompleteEndIndex())) {
                errorResult.addReplacement(contents.substring(
                    nowikiTag.getValueBeginIndex() + index,
                    nowikiTag.getValueBeginIndex() + tmpIndex));
                List<String[]> isbnTemplates = analysis.getWPCConfiguration().getStringArrayList(
                    WPCConfigurationStringList.ISBN_TEMPLATES);
                if (isbnTemplates != null) {
                  for (String[] isbnTemplate : isbnTemplates) {
                    if (isbnTemplate.length > 2) {
                      String templateName = isbnTemplate[0];
                      String[] params = isbnTemplate[1].split(",");
                      Boolean suggested = Boolean.valueOf(isbnTemplate[2]);
                      if ((params.length > 0) && (Boolean.TRUE.equals(suggested))) {
                        TemplateBuilder builder = TemplateBuilder.from(templateName);
                        builder.addParam(
                            !"1".equals(params[0]) ? params[0] : null,
                                nowikiContent.substring(indexCharacter, tmpIndex));
                        errorResult.addReplacement(builder.toString());
                      }
                    }
                  }
                }
              }
              errors.add(errorResult);
              index = tmpIndex;
            } else {
              index += PageElementISBN.ISBN_PREFIX.length();
            }
          } else {
            index++;
          }
        }
      }
    }

    return result;
  }

  /**
   * Analyze interwiki links to check if an ISBN is present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeInterwikiLinks(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {

    List<PageElementInterwikiLink> iwLinks = analysis.getInterwikiLinks();
    if (iwLinks == null) {
      return false;
    }

    boolean result = false;
    for (PageElementInterwikiLink iwLink : iwLinks) {
      String link = iwLink.getLink();
      int anchorIndex = link.indexOf(':');
      if (anchorIndex > 0) {
        String namespace = link.substring(0, anchorIndex);
        String iwText = iwLink.getInterwikiText();
        Pair<Set<String>, Set<String>> wiki = BOOK_SOURCES.get(iwText);
        if ("Special".equals(namespace) ||
            ((wiki != null) &&
             (wiki.getLeft() != null) &&
             wiki.getLeft().contains(namespace))) {
          String target = link.substring(anchorIndex + 1);
          int slashIndex = target.indexOf('/');
          if (slashIndex > 0) {
            target = target.substring(0, slashIndex);
          }
          target.replaceAll("_", " ");
          if ("BookSources".equals(target) ||
              ((wiki != null) &&
               (wiki.getRight() != null) &&
               (wiki.getRight().contains(target)))) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, iwLink.getBeginIndex(), iwLink.getEndIndex());
            if (iwLink.getText() != null) {
              errorResult.addReplacement(iwLink.getText());
            }
            errors.add(errorResult);
          }
        }
      }
    }

    return result;
  }

  /**
   * @param analysis Page analysis.
   * @param link Link destination.
   * @return Error level.
   */
  private ErrorLevel isSpecialBookSources(PageAnalysis analysis, String link) {
    if (link == null) {
      return ErrorLevel.CORRECT;
    }
    int colonIndex = link.indexOf(':');
    if (colonIndex == 0) {
      link = link.substring(1);
      colonIndex = link.indexOf(':');
    }
    if (colonIndex > 0) {
      Namespace special = analysis.getWikiConfiguration().getNamespace(Namespace.SPECIAL);
      String prefix = link.substring(0, colonIndex);
      if ((special != null) && (special.isPossibleName(prefix))) {
        if (link.startsWith("BookSources", colonIndex + 1)) {
          return ErrorLevel.ERROR;
        }
        return ErrorLevel.WARNING;
      }
    }
    return ErrorLevel.CORRECT;
  }

  /**
   * @param isbn ISBN number.
   * @return Reason for the error.
   */
  @Override
  public String getReason(PageElementISBN isbn) {
    if (isbn == null) {
      return null;
    }
    return reason;
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Reason for the error */
  private static final String PARAMETER_REASON = "reason";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    reason = getSpecificProperty(PARAMETER_REASON, true, true, false);
  }

  /** Reason for the error */
  private String reason = null;

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_REASON,
        GT._T("An explanation of the problem"),
        new AlgorithmParameterElement(
            "text",
            GT._T("An explanation of the problem"))));
  }
}
