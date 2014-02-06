# A Liquid tag for Jekyll sites that allows the embedding of showterm.io
# recorded terminal sessions.
#
# Author: Jacky Alcine <me@jalcine.me>
# Source URL: https://gist.github.com/jalcine/6333481
#
# Example usage:
#   {% showterm <ID> %}
#

module Jekyll
  class ShowTermTag < Liquid::Tag
    def initialize(tag_name, id, tokens)
      super
      @id = id.strip!
    end

    def render(context)
      "<iframe src=\"http://showterm.io/#{@id}#stop\" width=\"100%\" height=\"480\" frameborder=\"no\"></iframe>"
    end
  end
end

Liquid::Template.register_tag('showterm', Jekyll::ShowTermTag)
