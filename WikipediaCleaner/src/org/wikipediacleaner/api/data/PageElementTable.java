/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.comment.ContentsComment;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Class containing information about a table.
 */
public class PageElementTable extends PageElement {

  /**
   * @param analysis Page analysis.
   * @return List of tables.
   */
  public static List<PageElementTable> analyzePage(
      PageAnalysis analysis) {
    List<PageElementTable> tables = new ArrayList<>();

    // Analysis for possible tables
    List<Integer> starts = listPossibleStarts(analysis);
    List<Integer> ends = listPossibleEnds(analysis);
    for (int startIndex = starts.size(); startIndex > 0; startIndex--) {
      Integer start = starts.get(startIndex - 1);
      boolean done = false;
      int endIndex = 0;
      while (!done && (endIndex < ends.size())) {
        Integer end = ends.get(endIndex);
        if (end.intValue() > start.intValue()) {
          PageElementTable table = analyzeTable(
              analysis, tables,
              start.intValue(), end.intValue());
          if (table != null) {
            tables.add(table);
            done = true;
          }
          ends.remove(endIndex);
        } else {
          endIndex++;
        }
      }
    }

    return tables;
  }

  /**
   * @param analysis Page analysis.
   * @param tables Tables already found in the page.
   * @param beginIndex Begin index.
   * @param endIndex End index.
   * @return Table.
   */
  private static PageElementTable analyzeTable(
      PageAnalysis analysis, List<PageElementTable> tables,
      int beginIndex, int endIndex) {

    String contents = analysis.getContents();

    // Analyze table start
    int currentIndex = beginIndex;
    if (contents.charAt(currentIndex) != '{') {
      return null;
    }
    currentIndex = getMeaningfulIndex(analysis, currentIndex + 1);
    if (contents.charAt(currentIndex) != '|') {
      return null;
    }
    currentIndex = getTrueIndex(analysis, null, currentIndex + 1);
    while ((currentIndex < endIndex) &&
           (contents.charAt(currentIndex) != '\n')) {
      currentIndex = getTrueIndex(analysis, null, currentIndex + 1);
    }
    if ((currentIndex >= endIndex) ||
        (contents.charAt(currentIndex) != '\n')) {
      return null;
    }
    TableStart start = new TableStart(beginIndex, currentIndex);

    // Analyze optional table caption
    while ((currentIndex < endIndex) &&
           ("\n ".indexOf(contents.charAt(currentIndex)) >= 0)) {
      currentIndex = getTrueIndex(analysis, null, currentIndex + 1);
    }
    TableCaption caption = null;
    if ((currentIndex < endIndex) &&
        (contents.charAt(currentIndex) == '|')) {
      int tmpIndex = getTrueIndex(analysis, null, currentIndex + 1);
      if ((tmpIndex < endIndex) &&
          (contents.charAt(tmpIndex) == '+')) {
        tmpIndex = getTrueIndex(analysis, null, tmpIndex  + 1);
        boolean endFound = false;
        while (!endFound) {
          while ((tmpIndex < endIndex) &&
                 (contents.charAt(tmpIndex) != '\n')) {
            tmpIndex = getTrueIndex(analysis, null, tmpIndex + 1);
          }
          int tmpIndex2 = getTrueIndex(analysis, null, tmpIndex + 1);
          while ((tmpIndex2 < endIndex) &&
                 (contents.charAt(tmpIndex2) == ' ')) {
            tmpIndex2 = getTrueIndex(analysis, null, tmpIndex2 + 1);
          }
          if (tmpIndex2 >= endIndex) {
            return null;
          }
          if ("|!".indexOf(contents.charAt(tmpIndex2)) >= 0) {
            endFound = true;
          } else {
            tmpIndex = tmpIndex2;
          }
        }
        if ((tmpIndex >= endIndex) ||
            (contents.charAt(tmpIndex) != '\n')) {
          return null;
        }
        caption = new TableCaption(currentIndex, tmpIndex);
        currentIndex = tmpIndex;
      }
    }

    // Analyze table lines
    List<TableLine> lines = new ArrayList<>();
    boolean lineFound = false;
    do {

      // Analyze new line
      while ((currentIndex < endIndex) &&
             ("\n ".indexOf(contents.charAt(currentIndex)) >= 0)) {
        currentIndex = getTrueIndex(analysis, null, currentIndex + 1);
      }
      lineFound = false;
      int newLineBegin = currentIndex;
      int newLineEnd = newLineBegin;
      int tmpIndex = newLineBegin;
      if (contents.startsWith("|-", newLineBegin)) {
        lineFound = true;
        while ((tmpIndex < endIndex) &&
               (contents.charAt(tmpIndex) != '\n')) {
          tmpIndex = getTrueIndex(analysis, null, tmpIndex + 1);
        }
        if (tmpIndex > endIndex) {
          return null;
        }
        newLineEnd = tmpIndex;
      } else {
        if (!contents.startsWith("|}", newLineBegin)) {
          if (!lines.isEmpty()) {
            return null;
          }
          if ("|!".indexOf(contents.charAt(newLineBegin)) < 0) {
            return null;
          }
        }
      }

      // Analyze line
      boolean endLineFound = false;
      List<TableCell> cells = new ArrayList<>();
      boolean newLine = true;
      while ((tmpIndex < endIndex) && !endLineFound) {

        // Go to the beginning of the cell
        while ((tmpIndex < endIndex) &&
               ("\n ".indexOf(contents.charAt(tmpIndex)) >= 0)) {
          tmpIndex = getTrueIndex(analysis, null, tmpIndex + 1);
        }
        char lineBeginChar = contents.charAt(tmpIndex);
        if ("|!".indexOf(lineBeginChar) < 0) {
          return null;
        }
        if (newLine) {
          if ((lineBeginChar == '|') &&
              ("-}".indexOf(contents.charAt(getMeaningfulIndex(analysis, tmpIndex + 1))) >= 0)) {
            TableLine line = new TableLine(newLineBegin, tmpIndex, newLineBegin, newLineEnd, cells);
            lines.add(line);
            endLineFound = true;
          }
        } else {
          if (contents.charAt(tmpIndex + 1) != lineBeginChar) {
            return null;
          }
        }

        // Analyze cell
        if (!endLineFound) {
          int cellBegin = tmpIndex;
          while ((tmpIndex < endIndex) &&
                 (contents.charAt(tmpIndex) == lineBeginChar)) {
            tmpIndex++;
          }
          int cellOptionsEnd = tmpIndex;
          boolean cellOptionsFound = false;
          tmpIndex = getTrueIndex(analysis, tables, tmpIndex);
          boolean endCellFound = false;
          String separators = (lineBeginChar == '!') ? "\n|!" : "\n|";
          while ((tmpIndex < endIndex) &&
                 (separators.indexOf(contents.charAt(tmpIndex)) < 0)) {
            tmpIndex = getTrueIndex(analysis, tables, tmpIndex + 1);
          }
          while ((tmpIndex < endIndex) && !endCellFound) {
            while (separators.indexOf(contents.charAt(tmpIndex)) < 0) {
              tmpIndex = getTrueIndex(analysis, tables, tmpIndex + 1);
            }
            char currentChar = contents.charAt(tmpIndex);
            if (currentChar == '|') {
              if (contents.charAt(tmpIndex + 1) == '|') {
                endCellFound = true;
              } else {
                tmpIndex++;
                if (!cellOptionsFound) {
                  cellOptionsFound = true;
                  cellOptionsEnd = tmpIndex;
                }
              }
            } else if (currentChar == '!') {
              if (contents.charAt(tmpIndex + 1) == '!') {
                endCellFound = true;
              } else {
                tmpIndex++;
              }
            } else if (currentChar == '\n') {
              cellOptionsFound = true;
              while ((tmpIndex < endIndex) &&
                     ("\n ".indexOf(contents.charAt(tmpIndex)) >= 0)) {
                tmpIndex = getMeaningfulIndex(analysis, tmpIndex + 1);
              }
              if ((tmpIndex < endIndex) &&
                  ("|!".indexOf(contents.charAt(tmpIndex)) >= 0)) {
                endCellFound = true;
              }
            }
          }
          if (endCellFound) {
            TableCell cell = new TableCell(cellBegin, cellOptionsEnd, tmpIndex);
            cells.add(cell);
            lineFound = true;
          }
        }
        currentIndex = tmpIndex;
      }
    } while (lineFound);

    // Analyze table end
    while ((currentIndex < endIndex) &&
           ("\n ".indexOf(contents.charAt(currentIndex)) >= 0)) {
      currentIndex = getTrueIndex(analysis, null, currentIndex + 1);
    }
    int tmpIndex = currentIndex;
    if (contents.charAt(tmpIndex) != '|') {
      return null;
    }
    tmpIndex = getMeaningfulIndex(analysis, tmpIndex + 1);
    if (contents.charAt(tmpIndex) != '}') {
      return null;
    }
    tmpIndex++;
    TableEnd end = new TableEnd(currentIndex, tmpIndex + 1);

    // Check if there's something strange between tmpIndex and endIndex ?
    while ((tmpIndex < endIndex) &&
           ("\n ".indexOf(contents.charAt(tmpIndex)) >= 0)) {
      tmpIndex = getTrueIndex(analysis, null, tmpIndex + 1);
    }
    if (tmpIndex < endIndex) {
      return null;
    }

    return new PageElementTable(
      beginIndex, endIndex, start, caption, lines, end);
  }

  /**
   * @param analysis Page analysis.
   * @return List of potential table starts.
   */
  private static List<Integer> listPossibleStarts(
      PageAnalysis analysis) {

    List<Integer> list = new ArrayList<>();
    String contents = analysis.getContents();
    int index = 0;
    boolean previousCr = true;
    while (index < contents.length()) {

      // Analyze the current character
      index = getMeaningfulIndex(analysis, index);
      if (index < contents.length()) {
        char currentChar = contents.charAt(index);
        if (currentChar == '\n') {
          previousCr = true;
        } else if (previousCr) {
          if (currentChar == '{') {
            int nextIndex = getMeaningfulIndex(analysis, index + 1);
            if ((nextIndex < contents.length()) &&
                (contents.charAt(nextIndex) == '|')) {
              list.add(Integer.valueOf(index));
            }
          }
          previousCr = false;
        }
        index++;
      }
    }
    return list;
  }

  /**
   * @param analysis Page analysis.
   * @return List of potential table ends.
   */
  private static List<Integer> listPossibleEnds(
      PageAnalysis analysis) {

    List<Integer> list = new ArrayList<>();
    String contents = analysis.getContents();
    int index = 0;
    boolean previousCr = true;
    while (index < contents.length()) {

      // Analyze the current character
      index = getMeaningfulIndex(analysis, index);
      if (index < contents.length()) {
        char currentChar = contents.charAt(index);
        if (currentChar == '\n') {
          previousCr = true;
        } else if (previousCr) {
          if (currentChar == '|') {
            int nextIndex = getMeaningfulIndex(analysis, index + 1);
            if ((nextIndex < contents.length()) &&
                (contents.charAt(nextIndex) == '}')) {
              list.add(Integer.valueOf(nextIndex + 1));
            }
          }
          if (currentChar != ' ') {
            previousCr = false;
          }
        }
        index++;
      }
    }
    return list;
  }

  /**
   * @param analysis Page analysis.
   * @param index Current index.
   * @return First meaningful character index starting at index.
   */
  private static int getMeaningfulIndex(
      PageAnalysis analysis, int index) {
    if (index < 0) {
      return 0;
    }
    String contents = analysis.getContents();
    if (index >= contents.length()) {
      return contents.length();
    }
    char currentChar = contents.charAt(index);
    if (currentChar == '<') {
      ContentsComment comment = analysis.comments().getBeginsAt(index);
      if (comment != null) {
        return getMeaningfulIndex(analysis, comment.getEndIndex());
      }
      PageElementTag tag = analysis.isInTag(index);
      if ((tag != null)  &&
          tag.isComplete() &&
          (tag.getBeginIndex() == index) &&
          WikiTagType.NOWIKI.equals(tag.getType())) {
        return getMeaningfulIndex(analysis, tag.getCompleteEndIndex());
      }
    }
    return index;
  }

  /**
   * @param analysis Page analysis.
   * @param tables Tables already found in the page.
   * @param index Current index.
   * @return First true character starting at index.
   */
  private static int getTrueIndex(
      PageAnalysis analysis, List<PageElementTable> tables, int index) {
    String contents = analysis.getContents();
    if (index >= contents.length()) {
      return index;
    }
    char currentChar = contents.charAt(index);
    if (currentChar == '<') {
      ContentsComment comment = analysis.comments().getBeginsAt(index);
      if (comment != null) {
        return getTrueIndex(analysis, tables, comment.getEndIndex());
      }
      PageElementTag tag = analysis.isInTag(index);
      if ((tag != null) && (tag.getBeginIndex() == index)) {
        if (WikiTagType.NOWIKI.equals(tag.getType()) ||
            WikiTagType.REF.equals(tag.getType())) {
          return getTrueIndex(analysis, tables, tag.getCompleteEndIndex());
        }
        return getTrueIndex(analysis, tables, tag.getEndIndex());
      }
    } else if (currentChar == '[') {
      PageElementInternalLink iLink = analysis.isInInternalLink(index);
      if ((iLink != null) && (iLink.getBeginIndex() == index)) {
        return getTrueIndex(analysis, tables, iLink.getEndIndex());
      }
      PageElementExternalLink eLink = analysis.isInExternalLink(index);
      if ((eLink != null) && (eLink.getBeginIndex() == index)) {
        return getTrueIndex(analysis, tables, eLink.getEndIndex());
      }
      PageElementLanguageLink lLink = analysis.isInLanguageLink(index);
      if ((lLink != null) && (lLink.getBeginIndex() == index)) {
        return getTrueIndex(analysis, tables, lLink.getEndIndex());
      }
      PageElementInterwikiLink iwLink = analysis.isInInterwikiLink(index);
      if ((iwLink != null) && (iwLink.getBeginIndex() == index)) {
        return getTrueIndex(analysis, tables, iwLink.getEndIndex());
      }
      PageElementCategory category = analysis.isInCategory(index);
      if ((category != null) && (category.getBeginIndex() == index)) {
        return getTrueIndex(analysis, tables, category.getEndIndex());
      }
      PageElementImage image = analysis.isInImage(index);
      if ((image != null) && (image.getBeginIndex() == index)) {
        return getTrueIndex(analysis, tables, image.getEndIndex());
      }
    } else if (currentChar == '{') {
      PageElementTemplate template = analysis.isInTemplate(index);
      if ((template != null) && (template.getBeginIndex() == index)) {
        return getTrueIndex(analysis, tables, template.getEndIndex());
      }
      PageElementFunction function = analysis.isInFunction(index);
      if ((function != null) && (function.getBeginIndex() == index)) {
        return getTrueIndex(analysis, tables, function.getEndIndex());
      }
      PageElementParameter parameter = analysis.isInParameter(index);
      if ((parameter != null) && (parameter.getBeginIndex() == index)) {
        return getTrueIndex(analysis, tables, parameter.getEndIndex());
      }
      PageElementTable table = PageElementTable.isInTable(index, tables);
      if ((table != null) && (table.getBeginIndex() == index)) {
        return getTrueIndex(analysis, tables, table.getEndIndex());
      }
    }

    return index;
  }

  /**
   * @param index Current index.
   * @param tables List of tables.
   * @return True if the current index is already in a table.
   */
  public static PageElementTable isInTable(int index, List<PageElementTable> tables) {
    if (tables != null) {
      for (PageElementTable tmpTable : tables) {
        if ((tmpTable.getBeginIndex() <= index) &&
            (tmpTable.getEndIndex() > index)) {
          return tmpTable;
        }
      }
    }
    return null;
  }

  /** Table start element */
  private final TableStart start;

  /** Table caption element */
  private final TableCaption caption;

  /** Table lines elements */
  private final List<TableLine> lines;

  /** Table end element */
  private final TableEnd end;

  /**
   * @param beginIndex Begin index.
   * @param endIndex End index.
   * @param start Table start.
   * @param caption Table caption.
   * @param lines Table lines.
   * @param end Table end.
   */
  private PageElementTable(
      int beginIndex, int endIndex,
      TableStart start, TableCaption caption,
      List<TableLine> lines, TableEnd end) {
    super(beginIndex, endIndex);
    this.start = start;
    this.caption = caption;
    this.lines = lines;
    this.end = end;
  }

  /**
   * @return Table start.
   */
  public TableStart getTableStart() {
    return start;
  }

  /**
   * @return Table caption.
   */
  public TableCaption getTableCaption() {
    return caption;
  }

  /**
   * @return Table lines.
   */
  public List<TableLine> getTableLines() {
    if (lines == null) {
      return null;
    }
    return Collections.unmodifiableList(lines);
  }

  /**
   * @param index Index in the text.
   * @return Line containing the given index.
   */
  public TableLine getLineAtIndex(int index) {
    if (lines != null) {
      for (TableLine line : lines) {
        if ((line.getBeginIndex() <= index) &&
            (line.getEndIndex() > index)) {
          return line;
        }
      }
    }
    return null;
  }

  /**
   * @param index Index in the text.
   * @return Cell containing the given index.
   */
  public TableCell getCellAtIndex(int index) {
    TableLine line = getLineAtIndex(index);
    if ((line != null) && (line.getCells() != null)) {
      for (TableCell cell : line.getCells()) {
        if ((cell.getBeginIndex() <= index) &&
            (cell.getEndIndex() > index)) {
          return cell;
        }
      }
    }
    return null;
  }

  /**
   * @return Table end.
   */
  public TableEnd getTableEnd() {
    return end;
  }

  /**
   * Class containing information about the table start.
   */
  public static class TableStart extends PageElement {

    TableStart(
        int beginIndex, int endIndex) {
      super(beginIndex, endIndex);
    }
  }

  /**
   * Class containing information about the table end.
   */
  public static class TableEnd extends PageElement {

    TableEnd(
        int beginIndex, int endIndex) {
      super(beginIndex, endIndex);
    }
  }

  /**
   * Class containing information about the table caption.
   */
  public static class TableCaption extends PageElement {

    TableCaption(
        int beginIndex, int endIndex) {
      super(beginIndex, endIndex);
    }
  }

  /**
   * Class containing information about a table line.
   */
  public static class TableLine extends PageElement {

    /** Index of the beginning of the new line */
    private final int newLineBeginIndex;

    /** Index of the end of the new line */
    private final int newLineEndIndex;

    /** Cells in the line */
    private final List<TableCell> cells;

    TableLine(
        int beginIndex, int endIndex,
        int newLineBeginIndex, int newLineEndIndex,
        List<TableCell> cells) {
      super(beginIndex, endIndex);
      this.newLineBeginIndex = newLineBeginIndex;
      this.newLineEndIndex = newLineEndIndex;
      this.cells = cells;
    }

    /**
     * @return Index of the beginning of the new line.
     */
    public int getNewLineBeginIndex() {
      return newLineBeginIndex;
    }

    /**
     * @return Index of the end of the new line.
     */
    public int getNewLineEndIndex() {
      return newLineEndIndex;
    }

    /**
     * @return List of cells in the line.
     */
    public List<TableCell> getCells() {
      if (cells == null) {
        return null;
      }
      return Collections.unmodifiableList(cells);
    }
  }

  /**
   * Class containing information about a table cell.
   */
  public static class TableCell extends PageElement {

    /** Index of the end of the cell options */
    private final int endOptionsIndex;

    TableCell(
        int beginIndex, int endOptionsIndex, int endIndex) {
      super(beginIndex, endIndex);
      this.endOptionsIndex = endOptionsIndex;
    }

    /**
     * @return Index of the end of the cell options.
     */
    public int getEndOptionsIndex() {
      return endOptionsIndex;
    }
  }
}