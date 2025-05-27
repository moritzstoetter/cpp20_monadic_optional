#pragma once

#include <concepts>
#include <optional>
#include <type_traits>
#include <utility>

namespace stdx {
template<typename Callable>
struct and_then {
  explicit constexpr and_then(Callable&& cb) : f{cb} {}

  private:
  Callable f;

  template<typename T, typename Self>
  friend constexpr auto operator|(std::optional<T> v,
                                  Self&& self) noexcept(std::is_nothrow_invocable_v<std::remove_cvref_t<Callable>, T>)
    -> std::invoke_result_t<std::remove_cvref_t<Callable>, T> {
    return v.has_value() ? std::forward<Self>(self).f(*v) : std::nullopt;
  }
};

template<typename Callable>
struct transform {
  explicit constexpr transform(Callable&& cb) : f{cb} {}

  private:
  Callable f;

  template<typename T, typename Self>
  friend constexpr auto operator|(std::optional<T> v,
                                  Self&& self) noexcept(std::is_nothrow_invocable_v<std::remove_cvref_t<Callable>, T>)
    -> std::optional<std::invoke_result_t<std::remove_cvref_t<Callable>, T>> {
    return v.has_value() ? std::optional{std::forward<Self>(self).f(*v)} : std::nullopt;
  }
};

template<typename Callable>
struct or_else {
  explicit or_else(Callable&& cb) : f{cb} {}

  private:
  Callable f;

  template<typename T, typename Self>
  friend constexpr auto operator|(std::optional<T> v,
                                  Self&& self) noexcept(std::is_nothrow_invocable_v<std::remove_cvref_t<Callable>, T>)
    -> std::invoke_result_t<std::remove_cvref_t<Callable>, T> {
    static_assert(std::same_as<T, std::invoke_result_t<std::remove_cvref_t<Callable>, T>>,
                  "Type of left operand does not match return type of callable");
    return v.has_value() ? *v : std::forward<Self>(self).f();
  }
};
}
