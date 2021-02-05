/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a50x.a501;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

import javax.annotation.Nonnull;

import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.Areas;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsInterval;
import org.wikipediacleaner.api.data.contents.Interval;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.TagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;

/**
 * Analyzer for extracting the chunks of text that can be checked.
 */
class ChunkAnalyzer {

  /**
   * Constructor.
   */
  public ChunkAnalyzer() {
  }

  /**
   * Split contents into analyzable chunks.
   * 
   * @param analysis Page analysis.
   * @param nativeRegexp True if creating chunks for WPCleaner regular expressions.
   * @return List of contents chunks.
   */
  @Nonnull
  public List<Interval> computeContentsChunks(
      @Nonnull PageAnalysis analysis,
      boolean nativeRegexp) {
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
    removeCompleteTags(chunks, analysis, HtmlTagType.CODE);
    removeCompleteTags(chunks, analysis, WikiTagType.GRAPH);
    removeCompleteTags(chunks, analysis, WikiTagType.IMAGEMAP); // TODO: keep descriptions
    removeCompleteTags(chunks, analysis, WikiTagType.MATH);
    removeCompleteTags(chunks, analysis, WikiTagType.MATH_CHEM);
    removeCompleteTags(chunks, analysis, WikiTagType.SCORE);
    removeCompleteTags(chunks, analysis, WikiTagType.SOURCE);
    removeCompleteTags(chunks, analysis, WikiTagType.SYNTAXHIGHLIGHT);
    removeCompleteTags(chunks, analysis, WikiTagType.TIMELINE);
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
   * @param tagType Tag type to remove.
   */
  private void removeCompleteTags(
      @Nonnull List<Interval> chunks,
      @Nonnull PageAnalysis analysis,
      @Nonnull TagType tagType) {
    List<PageElementTag> tags = analysis.getCompleteTags(tagType);
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
  private void removeGalleryTags(
      @Nonnull List<Interval> chunks,
      @Nonnull PageAnalysis analysis) {
    Namespace imageNamespace = analysis.getWikiConfiguration().getNamespace(Namespace.IMAGE);
    String contents = analysis.getContents();
    List<PageElementTag> tags = analysis.getCompleteTags(WikiTagType.GALLERY);
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
  private void removeArea(
      @Nonnull List<Interval> chunks,
      int begin, int end) {
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
}
