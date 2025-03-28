/// This property describes decorations that are added to the text of an element.
enum TextDecoration {
    Underline,
    Overline,
    LineThrough,
    Blink,
}


/// The ‘writing-mode’ property specifies whether the initial inline-progression-direction for a ‘text’ element shall be
/// left-to-right, right-to-left, or top-to-bottom. The ‘writing-mode’ property applies only to ‘text’ elements;
/// the property is ignored for ‘tspan’, ‘tref’, ‘altGlyph’ and ‘textPath’ sub-elements. (Note that the inline-progression-direction
/// can change within a ‘text’ element due to the Unicode bidirectional algorithm and properties ‘direction’ and ‘unicode-bidi’.
/// For more on bidirectional text, see Relationship with bidirectionality.)
enum WritingMode {
    /// Sets the initial inline-progression-direction to left-to-right, as is common in most Latin-based documents.
    /// For most characters, the current text position is advanced from left to right after each glyph is rendered.
    /// (When the character data includes characters which are subject to the Unicode bidirectional algorithm, the text
    /// advance rules are more complex. See Relationship with bidirectionality).
    LrTb,
    /// Sets the initial inline-progression-direction to right-to-left, as is common in Arabic or Hebrew scripts.
    /// (See Relationship with bidirectionality.)
    RlTb,
    /// Sets the initial inline-progression-direction to top-to-bottom, as is common in some Asian scripts,
    /// such as Chinese and Japanese. Though hardly as frequent as horizontal, this type of vertical layout also occurs
    /// in Latin based documents, particularly in table column or row labels. In most cases, the vertical baselines
    /// running through the middle of each glyph are aligned.
    TbRl,
    /// See [`LrTb`](WritingMode::LrTb)
    Lr,
    /// See [`RlTb`](WritingMode::RlTb)
    Rl,
    /// See [`TbRl`](WritingMode::TbRl)
    Tb,
}


/// support for various international writing directions, such as left-to-right (e.g., Latin scripts) and
/// bidirectional (e.g., Hebrew or Arabic) and vertical (e.g., Asian scripts).
class TextLayout {
    /// See [`WritingMode`]
    write_mode: WritingMode,
    /// See [`TextDirection`]
    direction: TextDirection,
}

/// Fill properties.
class Fill {
    foreground_color: color,
    background_color: color,
}

/// All fields of a `state` are default required.
state Label {
    /// The fields is a value object passed by value.
    text: string,
    /// optional content builder function.
    content: @option fn() -> view,
}

/// A platform provided view function to display a text characters
@platform
extern fn label(Label, TextLayout, Fill) -> view;

/// A platform provided view function to display a text characters.
/// 
/// The `text` parameter is a `StateObject` passed by reference.
@platform
extern fn text_field(text: @state string, TextLayout, Fill, content: @option fn() -> view) -> view;

