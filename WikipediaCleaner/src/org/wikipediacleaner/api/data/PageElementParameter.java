/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;


import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.contents.ContainerComment;
import org.wikipediacleaner.api.data.contents.ContentsComment;


/**
 * Class containing information about a parameter ({{{<i>parameter</i>|...}}). 
 */
public class PageElementParameter extends PageElement {

  private final String parameterName;
  private final String parameterNameNotTrimmed;
  private final List<Parameter> parameters;

  private final static String parameterNameUnauthorizedCharacters = "{}[]|<>";

  /**
   * Class containing information about a parameter parameter.
   */
  private static class Parameter {
    final int pipeIndex;
    final String name;
    final int nameStartIndex;
    final String value;
    final int valueStartIndex;

    /**
     * @param pipeIndex Index of the pipe "|" in page contents.
     * @param name Parameter name.
     * @param nameStartIndex Index of parameter name in page contents.
     * @param value Parameter value.
     * @param valueStartIndex Index of parameter value in page contents.
     */
    public Parameter(
        int pipeIndex,
        String name, int nameStartIndex,
        String value, int valueStartIndex) {
      this.pipeIndex = pipeIndex;
      this.name = (name != null) ? name.trim() : null;
      this.nameStartIndex = nameStartIndex;
      this.value = (value != null) ? value.trim() : null;
      this.valueStartIndex = valueStartIndex;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
      if ((name != null) && (!name.isEmpty())) {
        return name + "=" + value;
      }
      return value;
    }
  }

  /**
   * Analyze contents to check if it matches a block.
   * 
   * @param wiki Wiki.
   * @param contents Contents.
   * @param index Block start index.
   * @param comments Comments in the page.
   * @param tags Tags in the page.
   * @return Block details it there's a block.
   */
  public static PageElementParameter analyzeBlock(
      EnumWikipedia wiki,
      String contents, int index,
      ContainerComment comments,
      List<PageElementTag> tags) {
    // Verify arguments
    if (contents == null) {
      return null;
    }

    // Look for '{{{'
    int beginIndex = index;
    int tmpIndex = beginIndex;
    if ((tmpIndex >= contents.length() - 2) ||
        (contents.charAt(tmpIndex) != '{') ||
        (contents.charAt(tmpIndex + 1) != '{') ||
        (contents.charAt(tmpIndex + 2) != '{')) {
      return null;
    }
    tmpIndex += 3;

    boolean moved = false;
    do {
      moved = false;

      // Possible whitespace characters
      while ((tmpIndex < contents.length()) &&
             ((contents.charAt(tmpIndex) == ' ') ||
              (contents.charAt(tmpIndex) == '\n'))) {
        tmpIndex++;
        moved = true;
      }
  
      // Possible comment
      if ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == '<')) {
        ContentsComment comment = comments.getBeginsAt(tmpIndex);
        if (comment == null) {
          return null;
        }
        tmpIndex = comment.getEndIndex();
        moved = true;
      }
    } while (moved);

    int startParameterName = tmpIndex;

    // Retrieve parameter name
    while (tmpIndex < contents.length()) {
      char currentChar = contents.charAt(tmpIndex);
      if (parameterNameUnauthorizedCharacters.indexOf(currentChar) >= 0) {
        break;
      }
      tmpIndex++;
    }
    if (tmpIndex >= contents.length()) {
      return null;
    }
    String parameterName = contents.substring(startParameterName, tmpIndex).trim();
    if (parameterName.length() == 0) {
      return null;
    }

    do {
      moved = false;

      // Possible comment
      if ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == '<')) {
        ContentsComment comment = comments.getBeginsAt(tmpIndex);
        if (comment == null) {
          return null;
        }
        tmpIndex = comment.getEndIndex();
        moved = true;
      }
  
      // Possible whitespace characters
      while ((tmpIndex < contents.length()) &&
             ((contents.charAt(tmpIndex) == ' ') ||
              (contents.charAt(tmpIndex) == '\n'))) {
        tmpIndex++;
        moved = true;
      }
    } while (moved);

    // Check if it's a parameter without parameters
    if (contents.startsWith("}}}", tmpIndex)) {
      return new PageElementParameter(
          parameterName,
          beginIndex, tmpIndex + 3, null);
    }

    // Check if it's a parameter
    if (contents.charAt(tmpIndex) != '|') {
      return null;
    }

    // Analyze parameters
    tmpIndex++;
    List<Parameter> parameters = new ArrayList<Parameter>();
    int endIndex = analyzeParameterParameters(
        wiki, contents, beginIndex, tmpIndex - 1, tmpIndex, parameters,
        comments, tags);
    if (endIndex < 0) {
      return null;
    }
    return new PageElementParameter(
        parameterName,
        beginIndex, endIndex, parameters);
  }

  /**
   * Analyze the parameters of parameter.
   * 
   * @param wiki Wiki.
   * @param contents Contents of the page.
   * @param beginIndex Start index of the parameter in the page.
   * @param pipeIndex Index of the previous pipe.
   * @param parametersBeginIndex Start index of the parameters in the page.
   * @param parameters Parameters.
   * @param comments Comments in the page.
   * @param tags Tags in the page.
   * @return Position of the end of the parameter, or -1 if no parameter was found.
   */
  private static int analyzeParameterParameters(
      EnumWikipedia wiki, String contents,
      int beginIndex, int pipeIndex, int parametersBeginIndex,
      List<Parameter> parameters,
      ContainerComment comments,
      List<PageElementTag> tags) {
    if (contents == null) {
      return -1;
    }
    int tmpIndex = parametersBeginIndex;
    int maxLength = contents.length();
    int depth2CurlyBrackets = 0;
    int depth3CurlyBrackets = 0;
    int depth2SquareBrackets = 0;
    int depthTagNoWiki = 0;
    int depthTagRef = 0;
    int parameterBeginIndex = parametersBeginIndex;
    int equalIndex = -1;
    while (tmpIndex < maxLength) {
      if (contents.startsWith("{{{", tmpIndex)) {
        // Possible start of a parameter
        tmpIndex += 3;
        if (depthTagNoWiki == 0) {
          depth3CurlyBrackets++;
        }
      } else if (contents.startsWith("{{", tmpIndex)) {
        // Possible start of nested template
        tmpIndex += 2;
        if (depthTagNoWiki == 0) {
          depth2CurlyBrackets++;
        }
      } else if (contents.startsWith("}}", tmpIndex)) {
        if (contents.startsWith("}}}", tmpIndex) &&
            (depth2CurlyBrackets <= 0) &&
            (depth3CurlyBrackets <= 0)) {
          tmpIndex += 3;
          if (depthTagNoWiki == 0) {
            if (depth2CurlyBrackets > 0) {
              return -1;
            }
            addParameter(
                parameters, pipeIndex,
                contents.substring(parameterBeginIndex, tmpIndex - 3),
                equalIndex - parameterBeginIndex,
                parameterBeginIndex);
            return tmpIndex;
          }
        } else if (contents.startsWith("}}}", tmpIndex) &&
                   (depth3CurlyBrackets > 0)) {
          tmpIndex += 3;
          if (depthTagNoWiki == 0) {
            depth3CurlyBrackets--;
          }
        } else {
          // Possible end of template
          tmpIndex += 2;
          if (depthTagNoWiki == 0) {
            if (depth2CurlyBrackets > 0) {
              depth2CurlyBrackets--;
            }
          }
        }
      } else if (contents.startsWith("[[", tmpIndex)) {
        // Possible start of nested internal links
        tmpIndex += 2;
        if (depthTagNoWiki == 0) {
          depth2SquareBrackets++;
        }
      } else if (contents.startsWith("]]", tmpIndex)) {
        // Possible end of nested internal link
        tmpIndex += 2;
        if (depthTagNoWiki == 0) {
          if (depth2SquareBrackets > 0) {
            depth2SquareBrackets--;
          } else {
            return -1;
          }
        }
      } else if (contents.startsWith("<", tmpIndex)) {
        // Possible start of a tag
        PageElementTag tag = null;
        if (tags != null) {
          for (PageElementTag tmpTag : tags) {
            if (tmpTag.getBeginIndex() == tmpIndex) {
              tag = tmpTag;
            }
          }
        }
        if (tag != null) {
          int count = 0;
          if (tag.isFullTag()) {
            count = 0;
          } else if (tag.isEndTag()) {
            count = -1;
          } else {
            count = 1;
          }
          if (PageElementTag.TAG_WIKI_NOWIKI.equals(tag.getName())) {
            depthTagNoWiki += count;
            if (depthTagNoWiki < 0) {
              depthTagNoWiki = 0;
            }
          } else if (PageElementTag.TAG_WIKI_REF.equals(tag.getName())) {
            if (depthTagNoWiki == 0) {
              depthTagRef += count;
              if (depthTagRef < 0) {
                depthTagRef = 0;
              }
            }
          }
          tmpIndex = tag.getEndIndex();
        } else {
          // Possible start of a comment
          ContentsComment comment = comments.getBeginsAt(tmpIndex);
          if (comment != null) {
            tmpIndex = comment.getEndIndex();
          } else {
            tmpIndex++;
          }
        }
      } else {
        if ((depth2CurlyBrackets <= 0) &&
            (depth3CurlyBrackets <= 0) &&
            (depth2SquareBrackets <= 0) &&
            (depthTagNoWiki <= 0) &&
            (depthTagRef <= 0)) {
          char currentChar = contents.charAt(tmpIndex);
          if (currentChar == '|') {
            // Separation with next parameter
            depth2CurlyBrackets = 0;
            depth3CurlyBrackets = 0;
            depth2SquareBrackets = 0;
            addParameter(
                parameters, pipeIndex,
                contents.substring(parameterBeginIndex, tmpIndex),
                equalIndex - parameterBeginIndex,
                parameterBeginIndex);
            pipeIndex = tmpIndex;
            tmpIndex++;
            parameterBeginIndex = tmpIndex;
            equalIndex = -1;
          } else if ((currentChar == '=') && (equalIndex < 0)) {
            equalIndex = tmpIndex;
            tmpIndex++;
          } else {
            tmpIndex++;
          }
        } else {
          tmpIndex++;
        }
      }
    }
    return -1;
  }

  /**
   * @param parameters List of parameters.
   * @param pipeIndex Index of "|".
   * @param parameter New parameter (name=value or value).
   * @param equalIndex Index of "=" in the parameter or < 0 if doesn't exist.
   * @param offset Offset of parameter start index in page contents.
   */
  private static void addParameter(
      List<Parameter> parameters,
      int pipeIndex, String parameter,
      int equalIndex, int offset) {
    if (equalIndex < 0) {
      int spaces = 0;
      while ((spaces < parameter.length()) && (Character.isWhitespace(parameter.charAt(spaces)))) {
        spaces++;
      }
      parameters.add(new Parameter(
          pipeIndex, "", offset + spaces, parameter, offset + spaces));
    } else {
      int spacesName = 0;
      while ((spacesName < equalIndex) && (Character.isWhitespace(parameter.charAt(spacesName)))) {
        spacesName++;
      }
      int spacesValue = equalIndex + 1;
      while ((spacesValue < parameter.length()) && (Character.isWhitespace(parameter.charAt(spacesValue)))) {
        spacesValue++;
      }
      parameters.add(new Parameter(
          pipeIndex,
          parameter.substring(0, equalIndex), offset + spacesName,
          parameter.substring(equalIndex + 1), offset + spacesValue));
    }
  }

  /**
   * @return Parameter name.
   */
  public String getParameterName() {
    return parameterName;
  }

  /**
   * Get parameter count.
   * 
   * @return Parameter count.
   */
  public int getParameterCount() {
    if (parameters == null) {
      return 0;
    }
    return parameters.size();
  }

  /**
   * Retrieve pipe offset.
   * 
   * @param index Parameter index.
   * @return Pipe offset.
   */
  public int getParameterPipeOffset(int index) {
    if ((index >= 0) && (index < parameters.size())) {
      return parameters.get(index).pipeIndex;
    }
    return 0;
  }

  /**
   * Retrieve parameter name.
   * 
   * @param index Parameter index.
   * @return Parameter name.
   */
  public String getParameterName(int index) {
    if ((index >= 0) && (index < parameters.size())) {
      return parameters.get(index).name;
    }
    return null;
  }

  /**
   * Retrieve parameter name offset.
   * 
   * @param index Parameter index.
   * @return Parameter name offset.
   */
  public int getParameterNameOffset(int index) {
    if ((index >= 0) && (index < parameters.size())) {
      return parameters.get(index).nameStartIndex;
    }
    return 0;
  }

  /**
   * Retrieve parameter value.
   * 
   * @param index Parameter index.
   * @return Parameter value.
   */
  public String getParameterValue(int index) {
    if ((index >= 0) && (index < parameters.size())) {
      return parameters.get(index).value;
    }
    return null;
  }

  /**
   * Retrieve parameter value offset.
   * 
   * @param index Parameter index.
   * @return Parameter value offset.
   */
  public int getParameterValueOffset(int index) {
    if ((index >= 0) && (index < parameters.size())) {
      return parameters.get(index).valueStartIndex;
    }
    return 0;
  }

  /**
   * Retrieve parameter value.
   * 
   * @param name Parameter name.
   * @return Parameter value.
   */
  public String getParameterValue(String name) {
    if (parameters == null) {
      return null;
    }
    int index = 0;
    int paramNum = 1;
    while (index < parameters.size()) {
      String paramName = parameters.get(index).name;
      if ((paramName == null) || (paramName.length() == 0)) {
        paramName = Integer.toString(paramNum);
      }
      if (paramName.equals(Integer.toString(paramNum))) {
        paramNum++;
      }
      if (name.equals(paramName)) {
        return parameters.get(index).value;
      }
      index++;
    }
    return null;
  }

  private PageElementParameter(
      String parameterName,
      int beginIndex, int endIndex,
      List<Parameter> parameters) {
    super(beginIndex, endIndex);
    this.parameterNameNotTrimmed = parameterName;
    this.parameterName = (parameterName != null) ? parameterName.trim() : null;
    this.parameters = parameters;
  }

  private void addPartBeforeParameters(StringBuilder sb) {
    sb.append("{{");
    sb.append(parameterNameNotTrimmed);
  }

  private void addPartFromParameters(StringBuilder sb) {
    for (Parameter parameter : parameters) {
      addParameter(sb, parameter.name, parameter.value);
    }
    sb.append("}}");
  }

  private void addParameter(StringBuilder sb, String paramName, String parameterValue) {
    sb.append('|');
    if ((paramName != null) && (paramName.trim().length() > 0)) {
      sb.append(paramName);
      sb.append('=');
    }
    sb.append(parameterValue);
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    addPartBeforeParameters(sb);
    addPartFromParameters(sb);
    return sb.toString();
  }
}
