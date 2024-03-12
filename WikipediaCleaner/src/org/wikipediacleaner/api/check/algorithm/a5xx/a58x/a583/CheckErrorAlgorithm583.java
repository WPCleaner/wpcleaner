/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a58x.a583;

import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.Nonnull;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.magicword.ImageMagicWordType;
import org.wikipediacleaner.api.data.contents.magicword.MagicWord;
import org.wikipediacleaner.api.data.contents.magicword.MagicWordType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.api.data.contents.tag.gallery.GalleryTag;
import org.wikipediacleaner.api.data.contents.tag.gallery.GalleryTagAnalyzer;
import org.wikipediacleaner.api.data.contents.tag.gallery.GalleryTagLine;
import org.wikipediacleaner.api.data.contents.tag.gallery.GalleryTagLineOption;


/**
 * Algorithm for analyzing error 583 of check wikipedia project.
 * <br>
 * Error 583: Bogus image options in gallery tags (see [[Special:LintErrors/bogus-image-options]])
 */
public class CheckErrorAlgorithm583 extends CheckErrorAlgorithmBase {

  @Nonnull private static final Logger log = LoggerFactory.getLogger(CheckErrorAlgorithm583.class);

  private static final Set<MagicWordType> FORBIDDEN_MAGIC_WORDS = Stream
      .of(ImageMagicWordType.IMG_WIDTH, ImageMagicWordType.IMG_THUMBNAIL)
      .collect(Collectors.toSet());

  public CheckErrorAlgorithm583() {
    super("Bogus image option in gallery");
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

    // Check each gallery tag
    List<PageElementTag> galleryTags = analysis.getCompleteTags(WikiTagType.GALLERY);
    if (galleryTags.isEmpty()) {
      return false;
    }
    boolean result = false;
    GalleryTagAnalyzer analyzer = new GalleryTagAnalyzer(getWikiConfiguration(), analysis.getContents());
    for (PageElementTag galleryTag : galleryTags) {
      result |= analyzeGalleryTag(analysis, errors, analyzer, galleryTag);
    }

    return result;
  }

  /**
   * Analyze a gallery tag to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param analyzer Analyzer for gallery tags.
   * @param tag Gallery tag to be analyzed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeGalleryTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      GalleryTagAnalyzer analyzer,
      PageElementTag tag) {
    final GalleryTag galleryTag = analyzer.analyze(tag);
    boolean result = false;
    for (GalleryTagLine line : galleryTag.getLines()) {
      boolean hasEmptyOption = false;
      boolean hasDescription = false;
      for (GalleryTagLineOption option : line.getOptions()) {
        final String optionText = option.getOption().trim();
        final MagicWord magicWord = getWikiConfiguration().getImgMagicWord(optionText);
        if (optionText.isEmpty()) {
          if (hasEmptyOption) {
            hasDescription = true;
          }
          hasEmptyOption = true;
        } else {
          hasDescription |= (magicWord == null);
        }
        if (isForbiddenOption(magicWord)) {
          CheckErrorResult error = createCheckErrorResult(analysis, option.getBeginIndex(), option.getEndIndex());
          error.addReplacement("", analysis.isInImage(option.getBeginIndex()) == null);
          errors.add(error);
          result = true;
        }
      }
      if (hasEmptyOption && hasDescription) {
        boolean found = false;
        for (GalleryTagLineOption option : line.getOptions()) {
          if (option.getOption().trim().isEmpty() && !found) {
            found = true;
            CheckErrorResult error = createCheckErrorResult(analysis, option.getBeginIndex(), option.getEndIndex());
            PageElementTemplate template = analysis.isInTemplate(option.getBeginIndex());
            error.addReplacement("", template == null);
            errors.add(error);
            result = true;
          }
        }
      }
    }
    return result;
  }

  private boolean isForbiddenOption(final MagicWord magicWord) {
    if (magicWord == null) {
      return false;
    }
    return FORBIDDEN_MAGIC_WORDS.contains(magicWord.getType());
  }
  
  /**
   * Automatic fixing of all the errors in the page.
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
}
