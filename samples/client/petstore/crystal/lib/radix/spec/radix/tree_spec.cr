require "../spec_helper"

# Silence deprecation warnings when running specs and allow
# capture them for inspection.
module Radix
  class Tree(T)
    @show_deprecations = false
    @stderr : IO::Memory?

    def show_deprecations!
      @show_deprecations = true
    end

    private def deprecation(message)
      if @show_deprecations
        @stderr ||= IO::Memory.new
        @stderr.not_nil!.puts message
      end
    end
  end
end

# Simple Payload class
record Payload

module Radix
  describe Tree do
    context "a new instance" do
      it "contains a root placeholder node" do
        tree = Tree(Symbol).new
        tree.root.should be_a(Node(Symbol))
        tree.root.payload?.should be_falsey
        tree.root.placeholder?.should be_true
      end
    end

    describe "#add" do
      context "on a new instance" do
        it "replaces placeholder with new node" do
          tree = Tree(Symbol).new
          tree.add "/abc", :abc
          tree.root.should be_a(Node(Symbol))
          tree.root.placeholder?.should be_false
          tree.root.payload?.should be_truthy
          tree.root.payload.should eq(:abc)
        end
      end

      context "shared root" do
        it "inserts properly adjacent nodes" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/a", :a
          tree.add "/bc", :bc

          # /    (:root)
          # +-bc (:bc)
          # \-a  (:a)
          tree.root.children.size.should eq(2)
          tree.root.children[0].key.should eq("bc")
          tree.root.children[0].payload.should eq(:bc)
          tree.root.children[1].key.should eq("a")
          tree.root.children[1].payload.should eq(:a)
        end

        it "inserts nodes with shared parent" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/abc", :abc
          tree.add "/axyz", :axyz

          # /       (:root)
          # +-a
          #   +-xyz (:axyz)
          #   \-bc  (:abc)
          tree.root.children.size.should eq(1)
          tree.root.children[0].key.should eq("a")
          tree.root.children[0].children.size.should eq(2)
          tree.root.children[0].children[0].key.should eq("xyz")
          tree.root.children[0].children[1].key.should eq("bc")
        end

        it "inserts multiple parent nodes" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/admin/users", :users
          tree.add "/admin/products", :products
          tree.add "/blog/tags", :tags
          tree.add "/blog/articles", :articles

          # /                 (:root)
          # +-admin/
          # |      +-products (:products)
          # |      \-users    (:users)
          # |
          # +-blog/
          #       +-articles  (:articles)
          #       \-tags      (:tags)
          tree.root.children.size.should eq(2)
          tree.root.children[0].key.should eq("admin/")
          tree.root.children[0].payload?.should be_falsey
          tree.root.children[0].children[0].key.should eq("products")
          tree.root.children[0].children[1].key.should eq("users")
          tree.root.children[1].key.should eq("blog/")
          tree.root.children[1].payload?.should be_falsey
          tree.root.children[1].children[0].key.should eq("articles")
          tree.root.children[1].children[0].payload?.should be_truthy
          tree.root.children[1].children[1].key.should eq("tags")
          tree.root.children[1].children[1].payload?.should be_truthy
        end

        it "inserts multiple nodes with mixed parents" do
          tree = Tree(Symbol).new
          tree.add "/authorizations", :authorizations
          tree.add "/authorizations/:id", :authorization
          tree.add "/applications", :applications
          tree.add "/events", :events

          # /
          # +-events               (:events)
          # +-a
          #   +-uthorizations      (:authorizations)
          #   |             \-/:id (:authorization)
          #   \-pplications        (:applications)
          tree.root.children.size.should eq(2)
          tree.root.children[1].key.should eq("a")
          tree.root.children[1].children.size.should eq(2)
          tree.root.children[1].children[0].payload.should eq(:authorizations)
          tree.root.children[1].children[1].payload.should eq(:applications)
        end

        it "supports insertion of mixed routes out of order" do
          tree = Tree(Symbol).new
          tree.add "/user/repos", :my_repos
          tree.add "/users/:user/repos", :user_repos
          tree.add "/users/:user", :user
          tree.add "/user", :me

          # /user                (:me)
          #     +-/repos         (:my_repos)
          #     \-s/:user        (:user)
          #             \-/repos (:user_repos)
          tree.root.key.should eq("/user")
          tree.root.payload?.should be_truthy
          tree.root.payload.should eq(:me)
          tree.root.children.size.should eq(2)
          tree.root.children[0].key.should eq("/repos")
          tree.root.children[1].key.should eq("s/:user")
          tree.root.children[1].payload.should eq(:user)
          tree.root.children[1].children[0].key.should eq("/repos")
        end
      end

      context "mixed payloads" do
        it "allows node with different payloads" do
          payload1 = Payload.new
          payload2 = Payload.new

          tree = Tree(Payload | Symbol).new
          tree.add "/", :root
          tree.add "/a", payload1
          tree.add "/bc", payload2

          # /    (:root)
          # +-bc (payload2)
          # \-a  (payload1)
          tree.root.children.size.should eq(2)
          tree.root.children[0].key.should eq("bc")
          tree.root.children[0].payload.should eq(payload2)
          tree.root.children[1].key.should eq("a")
          tree.root.children[1].payload.should eq(payload1)
        end
      end

      context "dealing with unicode" do
        it "inserts properly adjacent parent nodes" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/日本語", :japanese
          tree.add "/素晴らしい", :amazing

          # /          (:root)
          # +-素晴らしい    (:amazing)
          # \-日本語      (:japanese)
          tree.root.children.size.should eq(2)
          tree.root.children[0].key.should eq("素晴らしい")
          tree.root.children[1].key.should eq("日本語")
        end

        it "inserts nodes with shared parent" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/日本語", :japanese
          tree.add "/日本は難しい", :japanese_is_difficult

          # /                (:root)
          # \-日本語            (:japanese)
          #     \-日本は難しい     (:japanese_is_difficult)
          tree.root.children.size.should eq(1)
          tree.root.children[0].key.should eq("日本")
          tree.root.children[0].children.size.should eq(2)
          tree.root.children[0].children[0].key.should eq("は難しい")
          tree.root.children[0].children[1].key.should eq("語")
        end
      end

      context "dealing with duplicates" do
        it "does not allow same path be defined twice" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/abc", :abc

          expect_raises Tree::DuplicateError do
            tree.add "/", :other
          end

          tree.root.children.size.should eq(1)
        end
      end

      context "dealing with catch all and named parameters" do
        it "prioritizes nodes correctly" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/*filepath", :all
          tree.add "/products", :products
          tree.add "/products/:id", :product
          tree.add "/products/:id/edit", :edit
          tree.add "/products/featured", :featured

          # /                      (:all)
          # +-products             (:products)
          # |        \-/
          # |          +-featured  (:featured)
          # |          \-:id       (:product)
          # |              \-/edit (:edit)
          # \-*filepath            (:all)
          tree.root.children.size.should eq(2)
          tree.root.children[0].key.should eq("products")
          tree.root.children[0].children[0].key.should eq("/")

          nodes = tree.root.children[0].children[0].children
          nodes.size.should eq(2)
          nodes[0].key.should eq("featured")
          nodes[1].key.should eq(":id")
          nodes[1].children[0].key.should eq("/edit")

          tree.root.children[1].key.should eq("*filepath")
        end

        it "does not split named parameters across shared key" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/:category", :category
          tree.add "/:category/:subcategory", :subcategory

          # /                         (:root)
          # +-:category               (:category)
          #           \-/:subcategory (:subcategory)
          tree.root.children.size.should eq(1)
          tree.root.children[0].key.should eq(":category")

          # inner children
          tree.root.children[0].children.size.should eq(1)
          tree.root.children[0].children[0].key.should eq("/:subcategory")
        end

        it "does allow same named parameter in different order of insertion" do
          tree = Tree(Symbol).new
          tree.add "/members/:id/edit", :member_edit
          tree.add "/members/export", :members_export
          tree.add "/members/:id/videos", :member_videos

          # /members/
          #         +-export      (:members_export)
          #         \-:id/
          #              +-videos (:members_videos)
          #              \-edit   (:members_edit)
          tree.root.key.should eq("/members/")
          tree.root.children.size.should eq(2)

          # first level children nodes
          tree.root.children[0].key.should eq("export")
          tree.root.children[1].key.should eq(":id/")

          # inner children
          nodes = tree.root.children[1].children
          nodes[0].key.should eq("videos")
          nodes[1].key.should eq("edit")
        end

        it "does not allow different named parameters sharing same level" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/:post", :post

          expect_raises Tree::SharedKeyError do
            tree.add "/:category/:post", :category_post
          end
        end
      end
    end

    describe "#find" do
      context "a single node" do
        it "does not find when using different path" do
          tree = Tree(Symbol).new
          tree.add "/about", :about

          result = tree.find "/products"
          result.found?.should be_false
        end

        it "finds when key and path matches" do
          tree = Tree(Symbol).new
          tree.add "/about", :about

          result = tree.find "/about"
          result.found?.should be_true
          result.key.should eq("/about")
          result.payload?.should be_truthy
          result.payload.should eq(:about)
        end

        it "finds when path contains trailing slash" do
          tree = Tree(Symbol).new
          tree.add "/about", :about

          result = tree.find "/about/"
          result.found?.should be_true
          result.key.should eq("/about")
        end

        it "finds when key contains trailing slash" do
          tree = Tree(Symbol).new
          tree.add "/about/", :about

          result = tree.find "/about"
          result.found?.should be_true
          result.key.should eq("/about/")
          result.payload.should eq(:about)
        end
      end

      context "nodes with shared parent" do
        it "finds matching path" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/abc", :abc
          tree.add "/axyz", :axyz

          result = tree.find("/abc")
          result.found?.should be_true
          result.key.should eq("/abc")
          result.payload.should eq(:abc)
        end

        it "finds matching path across separator" do
          tree = Tree(Symbol).new
          tree.add "/products", :products
          tree.add "/product/new", :product_new

          result = tree.find("/products")
          result.found?.should be_true
          result.key.should eq("/products")
          result.payload.should eq(:products)
        end

        it "finds matching path across parents" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/admin/users", :users
          tree.add "/admin/products", :products
          tree.add "/blog/tags", :tags
          tree.add "/blog/articles", :articles

          result = tree.find("/blog/tags/")
          result.found?.should be_true
          result.key.should eq("/blog/tags")
          result.payload.should eq(:tags)
        end
      end

      context "unicode nodes with shared parent" do
        it "finds matching path" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/日本語", :japanese
          tree.add "/日本日本語は難しい", :japanese_is_difficult

          result = tree.find("/日本日本語は難しい/")
          result.found?.should be_true
          result.key.should eq("/日本日本語は難しい")
        end
      end

      context "dealing with catch all" do
        it "finds matching path" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/*filepath", :all
          tree.add "/about", :about

          result = tree.find("/src/file.png")
          result.found?.should be_true
          result.key.should eq("/*filepath")
          result.payload.should eq(:all)
        end

        it "returns catch all in parameters" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/*filepath", :all
          tree.add "/about", :about

          result = tree.find("/src/file.png")
          result.found?.should be_true
          result.params.has_key?("filepath").should be_true
          result.params["filepath"].should eq("src/file.png")
        end

        it "returns optional catch all after slash" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/search/*extra", :extra

          result = tree.find("/search")
          result.found?.should be_true
          result.key.should eq("/search/*extra")
          result.params.has_key?("extra").should be_true
          result.params["extra"].empty?.should be_true
        end

        it "returns optional catch all by globbing" do
          tree = Tree(Symbol).new
          tree.add "/members*trailing", :members_catch_all

          result = tree.find("/members")
          result.found?.should be_true
          result.key.should eq("/members*trailing")
          result.params.has_key?("trailing").should be_true
          result.params["trailing"].empty?.should be_true
        end

        it "does not find when catch all is not full match" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/search/public/*query", :search

          result = tree.find("/search")
          result.found?.should be_false
        end

        it "does not find when path search has been exhausted" do
          tree = Tree(Symbol).new
          tree.add "/members/*trailing", :members_catch_all

          result = tree.find("/members2")
          result.found?.should be_false
        end

        it "does prefer specific path over catch all if both are present" do
          tree = Tree(Symbol).new
          tree.add "/members", :members
          tree.add "/members*trailing", :members_catch_all

          result = tree.find("/members")
          result.found?.should be_true
          result.key.should eq("/members")
        end

        it "does prefer catch all over specific key with partially shared key" do
          tree = Tree(Symbol).new
          tree.add "/orders/*anything", :orders_catch_all
          tree.add "/orders/closed", :closed_orders

          result = tree.find("/orders/cancelled")
          result.found?.should be_true
          result.key.should eq("/orders/*anything")
          result.params.has_key?("anything").should be_true
          result.params["anything"].should eq("cancelled")
        end
      end

      context "dealing with named parameters" do
        it "finds matching path" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/products", :products
          tree.add "/products/:id", :product
          tree.add "/products/:id/edit", :edit

          result = tree.find("/products/10")
          result.found?.should be_true
          result.key.should eq("/products/:id")
          result.payload.should eq(:product)
        end

        it "does not find partial matching path" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/products", :products
          tree.add "/products/:id/edit", :edit

          result = tree.find("/products/10")
          result.found?.should be_false
        end

        it "returns named parameters in result" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/products", :products
          tree.add "/products/:id", :product
          tree.add "/products/:id/edit", :edit

          result = tree.find("/products/10/edit")
          result.found?.should be_true
          result.params.has_key?("id").should be_true
          result.params["id"].should eq("10")
        end

        it "returns unicode values in parameters" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/language/:name", :language
          tree.add "/language/:name/about", :about

          result = tree.find("/language/日本語")
          result.found?.should be_true
          result.params.has_key?("name").should be_true
          result.params["name"].should eq("日本語")
        end

        it "does prefer specific path over named parameters one if both are present" do
          tree = Tree(Symbol).new
          tree.add "/tag-edit/:tag", :edit_tag
          tree.add "/tag-edit2", :alternate_tag_edit

          result = tree.find("/tag-edit2")
          result.found?.should be_true
          result.key.should eq("/tag-edit2")
        end

        it "does prefer named parameter over specific key with partially shared key" do
          tree = Tree(Symbol).new
          tree.add "/orders/:id", :specific_order
          tree.add "/orders/closed", :closed_orders

          result = tree.find("/orders/10")
          result.found?.should be_true
          result.key.should eq("/orders/:id")
          result.params.has_key?("id").should be_true
          result.params["id"].should eq("10")
        end
      end

      context "dealing with multiple named parameters" do
        it "finds matching path" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/:section/:page", :static_page

          result = tree.find("/about/shipping")
          result.found?.should be_true
          result.key.should eq("/:section/:page")
          result.payload.should eq(:static_page)
        end

        it "returns named parameters in result" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/:section/:page", :static_page

          result = tree.find("/about/shipping")
          result.found?.should be_true

          result.params.has_key?("section").should be_true
          result.params["section"].should eq("about")

          result.params.has_key?("page").should be_true
          result.params["page"].should eq("shipping")
        end
      end

      context "dealing with both catch all and named parameters" do
        it "finds matching path" do
          tree = Tree(Symbol).new
          tree.add "/", :root
          tree.add "/*filepath", :all
          tree.add "/products", :products
          tree.add "/products/:id", :product
          tree.add "/products/:id/edit", :edit
          tree.add "/products/featured", :featured

          result = tree.find("/products/1000")
          result.found?.should be_true
          result.key.should eq("/products/:id")
          result.payload.should eq(:product)

          result = tree.find("/admin/articles")
          result.found?.should be_true
          result.key.should eq("/*filepath")
          result.params["filepath"].should eq("admin/articles")

          result = tree.find("/products/featured")
          result.found?.should be_true
          result.key.should eq("/products/featured")
          result.payload.should eq(:featured)
        end
      end

      context "dealing with named parameters and shared key" do
        it "finds matching path" do
          tree = Tree(Symbol).new
          tree.add "/one/:id", :one
          tree.add "/one-longer/:id", :two

          result = tree.find "/one-longer/10"
          result.found?.should be_true
          result.key.should eq("/one-longer/:id")
          result.params["id"].should eq("10")
        end
      end
    end
  end
end
